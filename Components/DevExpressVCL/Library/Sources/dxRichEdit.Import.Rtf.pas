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

unit dxRichEdit.Import.Rtf;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections,
  dxCoreClasses, dxCore,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Import,
  dxCoreGraphics,

  dxRichEdit.NativeApi,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.Types,
  dxRichEdit.Options.Core,
  dxRichEdit.Options.Simple,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.Import.Formats,
  dxRichEdit.Import.Rtf.TableReader,
  dxRichEdit.Import.Rtf.ParagraphFormatting,
  dxGenerics,
  dxRichEdit.Import.Core;

type
  TdxRtfImporter = class;
  TdxRtfInputPosition = class;

  TdxRichEditRtfParsingState = (Normal, BinData, HexData);

  TdxRichEditPopRtfState = (StackNonEmpty, StackEmpty);

  { TdxRtfImageInfo }

  TdxRtfImageInfo = class
  strict private
    FDocumentModel: TdxDocumentModel;
    FRtfImage: TdxOfficeImageReference;
    FSizeInModelUnits: TSize;
    FScaleX: Integer;
    FScaleY: Integer;
    FPseudoInline: Boolean;
  private
    procedure SetRtfImage(const Value: TdxOfficeImageReference);
  protected
    procedure CreateImage(ANativeImage: TdxOfficeImage);
    property DocumentModel: TdxDocumentModel read FDocumentModel;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;

    procedure LoadMetafileFromStream(AStream: TBytesStream; AMapMode: TdxMapMode; APictureWidth, APictureHeight: Integer);
    procedure LoadImageFromStream(AStream: TBytesStream);
    procedure LoadDibFromStream(AStream: TBytesStream; AWidth, AHeight, ABytesInLine: Integer);
    procedure LoadBitmapFromStream(AStream: TBytesStream; AWidth, AHeight, AColorPlanesCount, ABitsPerPixel, ABytesInLine: Integer);

    property RtfImage: TdxOfficeImageReference read FRtfImage write SetRtfImage;
    property SizeInModelUnits: TSize read FSizeInModelUnits write FSizeInModelUnits;
    property ScaleX: Integer read FScaleX write FScaleX;
    property ScaleY: Integer read FScaleY write FScaleY;
    property PseudoInline: Boolean read FPseudoInline write FPseudoInline;
  end;

  { TdxRtfFontInfo }

  TdxRtfFontInfo = class
  private
    FCharset: Integer;
    FID: Integer;
    FName: string;
  public
    constructor Create;

    property Charset: Integer read FCharset write FCharset;
    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
  end;

  { TdxRtfFontInfoCollection }

  TdxRtfFontInfoCollection = class(TdxFastObjectList)
  private
    FDefaultRtfFontInfo: TdxRtfFontInfo;
    function CreateDefaultRtfFontInfo(ADocumentModel: TdxDocumentModel): TdxRtfFontInfo;
    function GetItem(Index: Integer): TdxRtfFontInfo;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;
    function GetRtfFontInfoById(AID: Integer): TdxRtfFontInfo;

    property DefaultRtfFontInfo: TdxRtfFontInfo read FDefaultRtfFontInfo;
    property Items[Index: Integer]: TdxRtfFontInfo read GetItem; default;
  end;

  { TdxKeywordTranslatorTable }

  TdxTranslateKeywordEvent = reference to procedure(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
  TdxKeywordTranslatorTable = class(TdxNamedDelegateDictionary<TdxTranslateKeywordEvent>);

  { TdxControlCharTranslatorTable }

  TdxTranslateControlCharEvent = reference to procedure(AImporter: TdxRtfImporter; var AChar: Char);
  TdxControlCharTranslatorTable = class(TDictionary<Char, TdxTranslateControlCharEvent>);

  { TdxRichEditRtfDestinationBase }

  TdxRichEditRtfDestinationBase = class
  public const
    macCodePage = 10000;
    pcCodePage  = 437;
    pcaCodePage = 850;
  strict private
    class var FDefaultKeywordHT: TdxKeywordTranslatorTable;
    class var FDefaultControlCharHT: TdxControlCharTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateDefaultControlCharHT: TdxControlCharTranslatorTable; static;
    class function CreateDefaultKeywordHT: TdxKeywordTranslatorTable; static;
  private
    FImporter: TdxRtfImporter;
    FNonEmpty: Boolean;
    FPieceTable: TdxPieceTable;

    class procedure ColorTableKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FontTableKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure UserTableKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure AnsiKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure MacKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure PcKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure PcaKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure AnsiCodePageKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure HyphAutoKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;

    class procedure OptionalGroupCharHandler(AImporter: TdxRtfImporter; var AChar: Char); static;
  protected
    function CanAppendText: Boolean; virtual;
    function CanProcessDefaultKeyword: Boolean; virtual;
    function CreateClone: TdxRichEditRtfDestinationBase; virtual; abstract;
    procedure IncreaseGroupLevel; virtual;

    procedure ProcessBinCharCore(AChar: Char); virtual;
    procedure ProcessCharCore(AChar: Char); virtual;
    procedure ProcessControlCharCore(AChar: Char); virtual;
    function ProcessKeywordCore(const AKeyword: string; AParameterValue: Integer; AHasParameter: Boolean): Boolean; virtual;
    procedure ProcessTextCore(const AText: string); virtual;

    procedure BeforeNestedGroupFinishedCore(ADestination: TdxRichEditRtfDestinationBase); virtual;
    function GetPieceTable: TdxPieceTable; virtual;

    class procedure BinKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure EscapedCharHandler(AImporter: TdxRtfImporter; var AChar: Char); static;
    class procedure SwitchToHexCharHandler(AImporter: TdxRtfImporter; var AChar: Char); static;
    class procedure UnicodeKeywordHandler(AImporter: TdxRtfImporter;
      AParameterValue: Integer; AHasParameter: Boolean); static;

    class function GetControlCharHT: TdxControlCharTranslatorTable; virtual;
    class function GetKeywordHT: TdxKeywordTranslatorTable; virtual;

    class property DefaultControlCharHT: TdxControlCharTranslatorTable read FDefaultControlCharHT;
    class property DefaultKeywordHT: TdxKeywordTranslatorTable read FDefaultKeywordHT;
    property ControlCharHT: TdxControlCharTranslatorTable read GetControlCharHT;
    property KeywordHT: TdxKeywordTranslatorTable read GetKeywordHT;

    property Importer: TdxRtfImporter read FImporter;
  public
    constructor Create(AImporter: TdxRtfImporter); virtual;
    destructor Destroy; override;

    procedure ProcessBinChar(AChar: Char);
    procedure ProcessChar(AChar: Char);
    procedure ProcessControlChar(AChar: Char);
    function ProcessKeyword(const AKeyword: string; AParameterValue: Integer; AHasParameter: Boolean): Boolean;
    procedure ProcessText(const AText: string);

    procedure AfterPopRtfState; virtual;
    procedure BeforePopRtfState; virtual;
    procedure BeforeNestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase);
    procedure NestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase); virtual;

    property NonEmpty: Boolean read FNonEmpty;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;
  TdxRichEditRtfDestinationBaseClass = class of TdxRichEditRtfDestinationBase;

  { TdxRtfFieldInfo }

  TdxRtfFieldInfo = class
  strict private
    FIsCodeView: Boolean;
    FIsLock: Boolean;
    FIsHyperlink: Boolean;
    FField: TdxField;
  public
    property IsCodeView: Boolean read FIsCodeView write FIsCodeView;
    property IsHyperlink: Boolean read FIsHyperlink write FIsHyperlink;
    property IsLock: Boolean read FIsLock write FIsLock;
    property Field: TdxField read FField write FField;
  end;

  { TdxRichEditRtfPieceTableInfo }


  TdxRichEditRtfPieceTableInfo = class(TdxImportPieceTableInfoBase)
  private
    FCharacterFormattingOptions: TdxCharacterFormattingOptions;
    FFields: TdxObjectStack<TdxRtfFieldInfo>;
    FParagraphTableStyles: TdxIntegersDictionary;
    FPosition: TdxRtfInputPosition;
    FTableReader: TdxRtfTableReader;
  public
    constructor Create(AImporter: TdxRtfImporter; APieceTable: TdxPieceTable);
    destructor Destroy; override;

    property Fields: TdxObjectStack<TdxRtfFieldInfo> read FFields;
    property ParagraphTableStyles: TdxIntegersDictionary read FParagraphTableStyles;
    property Position: TdxRtfInputPosition read FPosition;
    property TableReader: TdxRtfTableReader read FTableReader;
  end;
  TdxRichEditRtfPieceTableInfos = class(TdxObjectStack<TdxRichEditRtfPieceTableInfo>);

  { TdxCharacterDecoder }

  TdxCharacterDecoder = class
  private
    FEncoding: TEncoding;
  public
    constructor Create(ACodePage: Cardinal); virtual;

    property Encoding: TEncoding read FEncoding;
  end;

  { TdxCodePageCharacterDecoder }

  TdxCodePageCharacterDecoder = class(TdxCharacterDecoder)
  private
    FBytes: TBytes;
    FCapacity: Integer;
    FCount: Integer;
    procedure AddByte(Value: Byte);
    procedure Clear;
    procedure FlushByChar(AImporter: TdxRtfImporter; const AChars: TCharArray);
    procedure FlushByString(AImporter: TdxRtfImporter; const AChars: TCharArray);
  public
    constructor Create(ACodePage: Cardinal); override;
    destructor Destroy; override;

    procedure ProcessChar(AImporter: TdxRtfImporter; AChar: Char); virtual;
    procedure Flush(AImporter: TdxRtfImporter); virtual;
  end;

  { TdxEmptyCharacterDecoder }

  TdxEmptyCharacterDecoder = class(TdxCodePageCharacterDecoder)
  public
    constructor Create; reintroduce;
    procedure ProcessChar(AImporter: TdxRtfImporter; AChar: Char); override;
    procedure Flush(AImporter: TdxRtfImporter); override;
  end;

  { TdxRtfFormattingInfo }

  TdxRtfFormattingInfo = class(TdxReferencedObject)
  strict private
    FCodePage: Cardinal;
    FDecoder: TdxCodePageCharacterDecoder;
    FDeleted: Boolean;
    FParentStyleIndex: Integer;
    FUnicodeCharacterByteCount: Integer;
    procedure SetCodePageCore(Value: Cardinal);
    procedure SetCodePage(Value: Cardinal);
    procedure SetUnicodeCharacterByteCount(Value: Integer);
  protected
    function ChooseDecoder: TdxCodePageCharacterDecoder; virtual;
    procedure SetDecoder(Value: TdxCodePageCharacterDecoder; ANeedDestroyOldDecoder: Boolean = True);
  public
    constructor Create(ACodePage: Cardinal);
    destructor Destroy; override;
    procedure CopyFrom(const Source: TdxRtfFormattingInfo);

    function Clone: TdxRtfFormattingInfo;

    property CodePage: Cardinal read FCodePage write SetCodePage;
    property Decoder: TdxCodePageCharacterDecoder read FDecoder;
    property Deleted: Boolean read FDeleted write FDeleted;
    property ParentStyleIndex: Integer read FParentStyleIndex write FParentStyleIndex;
    property UnicodeCharacterByteCount: Integer read FUnicodeCharacterByteCount write SetUnicodeCharacterByteCount;
  end;
  TdxRtfFormattingInfoClass = class of TdxRtfFormattingInfo;

  { TdxRtfOldListLevelInfo }

  TdxRtfOldListLevelInfo = class
  private
    FCharacterProperties: TdxCharacterFormattingBase;
    FDocumentModel: TdxDocumentModel;
    FIncludeInformationFromPreviousLevel: Boolean;
    FIndent: Integer;
    FListLevelProperties: TdxListLevelInfo;
    FSkipNumbering: Boolean;
    FTextAfter: string;
    FTextBefore: string;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;
    procedure CopyFrom(const Source: TdxRtfOldListLevelInfo);
    function Clone: TdxRtfOldListLevelInfo;

    property CharacterProperties: TdxCharacterFormattingBase read FCharacterProperties;
    property IncludeInformationFromPreviousLevel: Boolean read FIncludeInformationFromPreviousLevel write FIncludeInformationFromPreviousLevel;
    property Indent: Integer read FIndent write FIndent;
    property ListLevelProperties: TdxListLevelInfo read FListLevelProperties;
    property SkipNumbering: Boolean read FSkipNumbering write FSkipNumbering;
    property TextAfter: string read FTextAfter write FTextAfter;
    property TextBefore: string read FTextBefore write FTextBefore;
  end;
  TdxRtfOldListLevelInfoClass = class of TdxRtfOldListLevelInfo;

  { TdxRtfOldListLevelInfoCollection }

  TdxRtfOldListLevelInfoCollection = class(TdxFastObjectList)
  private
    FDocumentModel: TdxDocumentModel;
    procedure EnsureLevelIndex(Index: Integer);
    function GetItem(Index: Integer): TdxRtfOldListLevelInfo;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);

    function Clone: TdxRtfOldListLevelInfoCollection;
    procedure CopyFrom(ASource: TdxRtfOldListLevelInfoCollection);

    property Items[Index: Integer]: TdxRtfOldListLevelInfo read GetItem; default;
  end;
  TdxRtfOldListLevelInfoCollectionClass = class of TdxRtfOldListLevelInfoCollection;

  { TdxRtfInputPositionState }

  TdxRtfFontType = (
    Undefined,
    DoubleByteCharactersFont,
    LowAnsiCharactersFont,
    HighAnsiCharactersFont);

  TdxRtfInputPositionState = class
  private
    FCharacterFormatting: TdxCharacterFormattingBase;
    FCharacterStyleIndex: Integer;
    FCurrentOldListLevelNumber: Integer;
    FCurrentOldListSkipNumbering: Boolean;
    FCurrentOldMultiLevelListIndex: TdxNumberingListIndex;
    FCurrentOldSimpleList: Boolean;
    FCurrentOldSimpleListIndex: TdxNumberingListIndex;
    FDoubleByteCharactersFontName: string;
    FLowAnsiCharactersFontName: string;
    FHighAnsiCharactersFontName: string;
    FFontType: TdxRtfFontType;
    FParagraphFormattingInfo: TdxRtfParagraphFormattingInfo;
    FParagraphFrameFormattingInfo: TdxParagraphFrameFormattingInfo;
    FRtfFormattingInfo: TdxRtfFormattingInfo;
    FOldListLevelInfo: TdxRtfOldListLevelInfo;
    FOldListLevelInfoCollection: TdxRtfOldListLevelInfoCollection;
    FTableStyleIndex: Integer;
  public
    constructor Create(APosition: TdxRtfInputPosition);
    destructor Destroy; override;

    property CharacterFormatting: TdxCharacterFormattingBase read FCharacterFormatting;
    property CharacterStyleIndex: Integer read FCharacterStyleIndex;
    property CurrentOldListLevelNumber: Integer read FCurrentOldListLevelNumber;
    property CurrentOldListSkipNumbering: Boolean read FCurrentOldListSkipNumbering;
    property CurrentOldMultiLevelListIndex: TdxNumberingListIndex read FCurrentOldMultiLevelListIndex;
    property CurrentOldSimpleList: Boolean read FCurrentOldSimpleList write FCurrentOldSimpleList;
    property CurrentOldSimpleListIndex: TdxNumberingListIndex read FCurrentOldSimpleListIndex;
    property DoubleByteCharactersFontName: string read FDoubleByteCharactersFontName;
    property ParagraphFrameFormattingInfo: TdxParagraphFrameFormattingInfo read FParagraphFrameFormattingInfo;
    property LowAnsiCharactersFontName: string read FLowAnsiCharactersFontName write FLowAnsiCharactersFontName;
    property HighAnsiCharactersFontName: string read FHighAnsiCharactersFontName write FHighAnsiCharactersFontName;
    property ParagraphFormattingInfo: TdxRtfParagraphFormattingInfo read FParagraphFormattingInfo;
    property RtfFormattingInfo: TdxRtfFormattingInfo read FRtfFormattingInfo;
    property FontType: TdxRtfFontType read FFontType write FFontType;
    property OldListLevelInfo: TdxRtfOldListLevelInfo read FOldListLevelInfo;
    property OldListLevelInfoCollection: TdxRtfOldListLevelInfoCollection read FOldListLevelInfoCollection;
    property TableStyleIndex: Integer read FTableStyleIndex;
  end;

  { TdxRtfSectionFormattingInfo }

  TdxRtfSectionFormattingInfo = class(TPersistent)
  private
    FColumns: TdxColumnsInfo;
    FCurrentColumnIndex: Integer;
    FGeneralSectionInfo: TdxGeneralSectionInfo;
    FFootNote: TdxSectionFootNote;
    FEndNote: TdxSectionFootNote;
    FLineNumbering: TdxLineNumberingInfo;
    FMargins: TdxMarginsInfo;
    FPage: TdxPageInfo;
    FPageNumbering: TdxPageNumberingInfo;
    FRestartPageNumbering: Boolean;
  protected
    procedure EnsureCurrentColumnExists;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure InitializeDefault(ADocumentModel: TdxDocumentModel);

    procedure SetCurrentColumnWidth(Value: Integer);
    procedure SetCurrentColumnSpace(Value: Integer);

    property Columns: TdxColumnsInfo read FColumns;
    property CurrentColumnIndex: Integer read FCurrentColumnIndex write FCurrentColumnIndex;
    property EndNote: TdxSectionFootNote read FEndNote;
    property GeneralSectionInfo: TdxGeneralSectionInfo read FGeneralSectionInfo;
    property FootNote: TdxSectionFootNote read FFootNote;
    property LineNumbering: TdxLineNumberingInfo read FLineNumbering;
    property Margins: TdxMarginsInfo read FMargins;
    property Page: TdxPageInfo read FPage;
    property PageNumbering: TdxPageNumberingInfo read FPageNumbering;
    property RestartPageNumbering: Boolean read FRestartPageNumbering write FRestartPageNumbering;
  end;

  { TdxRtfInputPosition }

  TdxRtfInputPosition = class(TdxInputPosition)
  private
    FCurrentOldListLevelNumber: Integer;
    FCurrentOldListSkipNumbering: Boolean;
    FCurrentOldMultiLevelListIndex: TdxNumberingListIndex;
    FCurrentOldSimpleList: Boolean;
    FCurrentOldSimpleListIndex: TdxNumberingListIndex;
    FDoubleByteCharactersFontName: string;
    FFieldInfo: TdxRtfFieldInfo;
    FFontType: TdxRtfFontType;
    FParagraphFrameFormattingInfo: TdxParagraphFrameFormattingInfo;
    FLowAnsiCharactersFontName: string;
    FHighAnsiCharactersFontName: string;
    FOldListLevelInfo: TdxRtfOldListLevelInfo;
    FOldListLevelInfoCollection: TdxRtfOldListLevelInfoCollection;
    FIsContainsParagraphFrame: Boolean;
    FIsOldListLevelInfoCollectionOwner: Boolean;
    FParagraphFormattingInfo: TdxRtfParagraphFormattingInfo;
    FRtfFormattingInfo: TdxRtfFormattingInfo;
    FSectionFormattingInfo: TdxRtfSectionFormattingInfo;
    FTableStyleIndex: Integer;
    FUseDoubleByteCharactersFontName: Boolean;
    FUseHighAnsiCharactersFontName: Boolean;
    FUseLowAnsiCharactersFontName: Boolean;
    function GetOldListLevelInfoCollection: TdxRtfOldListLevelInfoCollection;
    procedure SetOldListLevelInfo(const Value: TdxRtfOldListLevelInfo);
    procedure SetOldListLevelInfoCollection(const Value: TdxRtfOldListLevelInfoCollection);
    procedure SetParagraphFormattingInfo(const Value: TdxRtfParagraphFormattingInfo);
    procedure SetRtfFormattingInfo(const Value: TdxRtfFormattingInfo);
    procedure SetParagraphFrameFormattingInfo(const Value: TdxParagraphFrameFormattingInfo);
  protected
    procedure RecalcUseAssociatedProperties;
    procedure SetFont(const AFontName: string);
  public
    constructor Create(APieceTable: TdxSimplePieceTable; AImporter: TdxRtfImporter); reintroduce;
    destructor Destroy; override;

    function GetState: TdxRtfInputPositionState;
    procedure SetState(AState: TdxRtfInputPositionState);

    // for internal use
    procedure ResetUseAssociatedProperties;

    property CurrentOldMultiLevelListIndex: TdxNumberingListIndex read FCurrentOldMultiLevelListIndex write FCurrentOldMultiLevelListIndex;
    property CurrentOldSimpleListIndex: TdxNumberingListIndex read FCurrentOldSimpleListIndex write FCurrentOldSimpleListIndex;
    property CurrentOldListSkipNumbering: Boolean read FCurrentOldListSkipNumbering write FCurrentOldListSkipNumbering;
    property CurrentOldSimpleList: Boolean read FCurrentOldSimpleList write FCurrentOldSimpleList;
    property CurrentOldListLevelNumber: Integer read FCurrentOldListLevelNumber write FCurrentOldListLevelNumber;
    property DoubleByteCharactersFontName: string read FDoubleByteCharactersFontName write FDoubleByteCharactersFontName;
    property FieldInfo: TdxRtfFieldInfo read FFieldInfo;
    property FontType: TdxRtfFontType read FFontType write FFontType;
    property ParagraphFrameFormattingInfo: TdxParagraphFrameFormattingInfo read FParagraphFrameFormattingInfo write SetParagraphFrameFormattingInfo;
    property IsContainsParagraphFrame: Boolean read FIsContainsParagraphFrame write FIsContainsParagraphFrame;
    property LowAnsiCharactersFontName: string read FLowAnsiCharactersFontName write FLowAnsiCharactersFontName;
    property HighAnsiCharactersFontName: string read FHighAnsiCharactersFontName write FHighAnsiCharactersFontName;
    property OldListLevelInfo: TdxRtfOldListLevelInfo read FOldListLevelInfo write SetOldListLevelInfo;
    property OldListLevelInfoCollection: TdxRtfOldListLevelInfoCollection read GetOldListLevelInfoCollection;
    property ParagraphFormattingInfo: TdxRtfParagraphFormattingInfo read FParagraphFormattingInfo write SetParagraphFormattingInfo;
    property RtfFormattingInfo: TdxRtfFormattingInfo read FRtfFormattingInfo write SetRtfFormattingInfo;
    property SectionFormattingInfo: TdxRtfSectionFormattingInfo read FSectionFormattingInfo;
    property TableStyleIndex: Integer read FTableStyleIndex write FTableStyleIndex;
    property UseDoubleByteCharactersFontName: Boolean read FUseDoubleByteCharactersFontName write FUseDoubleByteCharactersFontName;
    property UseHighAnsiCharactersFontName: Boolean read FUseHighAnsiCharactersFontName write FUseHighAnsiCharactersFontName;
    property UseLowAnsiCharactersFontName: Boolean read FUseLowAnsiCharactersFontName write FUseLowAnsiCharactersFontName;
  end;

  { TdxRichEditRtfParserStateItem }

  TdxRichEditRtfParserStateItem = class
  private
    FDestination: TdxRichEditRtfDestinationBase;
    FInputPositionState: TdxRtfInputPositionState;
  public
    constructor Create(AInputPositionState: TdxRtfInputPositionState; ADestination: TdxRichEditRtfDestinationBase);

    property Destination: TdxRichEditRtfDestinationBase read FDestination;
    property InputPositionState: TdxRtfInputPositionState read FInputPositionState;
  end;
  TdxRichEditRtfParserStateItems = class(TdxObjectStack<TdxRichEditRtfParserStateItem>);

  { TdxRichEditRtfParserStateManager }

  TdxRichEditRtfParserStateManager = class
  private
    FDestination: TdxRichEditRtfDestinationBase;
    FSuperfluousDestinations: TdxFastObjectList;
    FImporter: TdxRtfImporter;
    FParsingState: TdxRichEditRtfParsingState;
    FPieceTables: TdxRichEditRtfPieceTableInfos;
    FStates: TdxRichEditRtfParserStateItems;
    function GetPieceTableInfo: TdxRichEditRtfPieceTableInfo;
    procedure SetDestination(const Value: TdxRichEditRtfDestinationBase);
  protected
    procedure CheckSuperfluousDestinations;
    procedure DestroyPieceTables;
    procedure DestroyStates;
    procedure Initialize;
    procedure SetDestinationCore(Value: TdxRichEditRtfDestinationBase; AUpdatePieceTableInfo: Boolean);

    property PieceTables: TdxRichEditRtfPieceTableInfos read FPieceTables;
  public
    constructor Create(AImporter: TdxRtfImporter);
    destructor Destroy; override;

    function CreateDefaultDestination: TdxRichEditRtfDestinationBase;
    procedure PopState;
    procedure PushState;

    property Destination: TdxRichEditRtfDestinationBase read FDestination write SetDestination;
    property Importer: TdxRtfImporter read FImporter;
    property ParsingState: TdxRichEditRtfParsingState read FParsingState write FParsingState;
    property PieceTableInfo: TdxRichEditRtfPieceTableInfo read GetPieceTableInfo;
    property States: TdxRichEditRtfParserStateItems read FStates;
  end;

  { TdxListLevelDisplayTextHelper }

  TdxListLevelDisplayTextHelper = class
  public
    class function CreateDisplayFormatStringCore(APlaceholderIndices: TdxIntegerList; const AText: string): string; static;
    class function GetTextRange(APlaceholderIndices: TdxIntegerList; AStartPlaceHolderIndex: Integer; const AText: string): string; static;
  end;

  { TdxRtfNumberingList }

  TdxRtfListID = Integer;
  TdxRtfNumberingListType = (Unknown, Hybrid, Simple);

  TdxRtfListLevel = class(TdxListLevel)
  private
    FText: string;
    FNumber: string;
  protected
    function CreatePlaceholderIndices: TdxIntegerList; virtual;
    function CreateDisplayFormatStringCore(APlaceholderIndices: TdxIntegerList): string; virtual;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); override;
    function CreateDisplayFormatString: string;

    procedure CopyFrom(AListLevel: TdxRtfListLevel); reintroduce;
    function Clone: TdxRtfListLevel; reintroduce;

    property Text: string read FText write FText;
    property Number: string read FNumber write FNumber;
  end;

  TdxRtfListLevels = TdxObjectList<TdxRtfListLevel>;

  TdxRtfNumberingList = class(TPersistent)
  private
    FID: TdxRtfListID;
    FLevels: TdxRtfListLevels;
    FName: string;
    FNumberingListType: TdxRtfNumberingListType;
    FParentStyleID: TdxRtfListID;
    FStyleName: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    property ID: TdxRtfListID read FID write FID;
    property Levels: TdxRtfListLevels read FLevels;
    property Name: string read FName write FName;
    property NumberingListType: TdxRtfNumberingListType read FNumberingListType write FNumberingListType;
    property ParentStyleID: TdxRtfListID read FParentStyleID write FParentStyleID;
    property StyleName: string read FStyleName write FStyleName;
  end;

  TdxRtfListTable = TdxObjectList<TdxRtfNumberingList>;

  TdxRtfListOverrideId = Integer;

  TdxRtfListOverrideLevel = class
  strict private
    FDocumentModel: TdxDocumentModel;
    FLevel: TdxRtfListLevel;
    FOverrideFormat: Boolean;
    FOverrideStartAt: Boolean;
    FStartAt: Integer;
    procedure SetLevel(const Value: TdxRtfListLevel);
  public
    constructor Create(ADocumentModel: TdxDocumentModel);

    procedure CopyFrom(ALevel: TdxRtfListOverrideLevel);
    function Clone: TdxRtfListOverrideLevel;

    property Level: TdxRtfListLevel read FLevel write SetLevel;
    property OverrideFormat: Boolean read FOverrideFormat write FOverrideFormat;
    property OverrideStartAt: Boolean read FOverrideStartAt write FOverrideStartAt;
    property StartAt: Integer read FStartAt write FStartAt;
  end;

  TdxRtfListOverrideLevels = TdxObjectList<TdxRtfListOverrideLevel>;

  TdxRtfNumberingListOverride = class
  strict private
    FID: TdxRtfListOverrideId;
    FListId: TdxRtfListId;
    FLevels: TdxRtfListOverrideLevels;
  public
    constructor Create;
    destructor Destroy; override;

    property ID: TdxRtfListOverrideId read FID write FID;
    property Levels: TdxRtfListOverrideLevels read FLevels;
    property ListId: TdxRtfListId read FListId write FListId;
  end;

  TdxListOverrideTable = TdxObjectList<TdxRtfNumberingListOverride>;

  { TdxRtfColorCollection }

  TdxRtfColorCollection = class(TdxAlphaColorList)
  public
    function GetRtfColorById(AID: Integer): TdxAlphaColor;
  end;

  { TdxRtfDocumentProperties }

  TdxRtfDocumentProperties = class
  strict private
    FColors: TdxRtfColorCollection;
    FDefaultCodePage: Integer;
    FDefaultFontNumber: Integer;
    FDefaultSectionProperties: TdxRtfSectionFormattingInfo;
    FFonts: TdxRtfFontInfoCollection;
    FListOverrideTable: TdxListOverrideTable;
    FListTable: TdxRtfListTable;
    FListTableComplete: Boolean;
    FListOverrideTableComplete: Boolean;
    FUserNames: TdxStringList;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;

    property Colors: TdxRtfColorCollection read FColors;
    property DefaultCodePage: Integer read FDefaultCodePage write FDefaultCodePage;
    property DefaultFontNumber: Integer read FDefaultFontNumber write FDefaultFontNumber;
    property DefaultSectionProperties: TdxRtfSectionFormattingInfo read FDefaultSectionProperties;
    property Fonts: TdxRtfFontInfoCollection read FFonts;
    property ListOverrideTable: TdxListOverrideTable read FListOverrideTable;
    property ListTable: TdxRtfListTable read FListTable;
    property ListTableComplete: Boolean read FListTableComplete write FListTableComplete;
    property ListOverrideTableComplete: Boolean read FListOverrideTableComplete write FListOverrideTableComplete;
    property UserNames: TdxStringList read FUserNames;
  end;

  { TdxRtfNumberingListInfo }

  TdxRtfNumberingListInfo = class
  strict private
    FRtfNumberingListIndex: Integer;
    FListLevelIndex: Integer;
  public
    constructor Create(ARtfNumberingListIndex, AListLevelIndex: Integer);
    property RtfNumberingListIndex: Integer read FRtfNumberingListIndex;
    property ListLevelIndex: Integer read FListLevelIndex;
  end;

  { TdxRtfImporter }

  TdxRtfImporter = class(TdxDocumentModelImporter)
  strict private
    FBinCharCount: Integer;
    FCharacterStyleCollectionIndex: TdxIntegersDictionary;
    FDocumentProperties: TdxRtfDocumentProperties;
    FKeyword: TStringBuilder;
    FIsContainsParagraphFrame: Boolean;
    FLastParagraphFormatting: TdxRtfParagraphFormattingInfo;
    FLastParagraphPropertiesCacheIndex: Integer;
    FLinkParagraphStyleIndexToCharacterStyleIndex: TdxIntegersDictionary;
    FListOverrideIndexToNumberingListIndexMap: TDictionary<Integer, TdxNumberingListIndex>;
    FNextParagraphStyleIndexTable: TdxIntegersDictionary;
    FNumberingListToOldListLevelInfoMap: TDictionary<TdxNumberingListIndex, TdxRtfOldListLevelInfo>;
    FOptionalGroupLevel: Integer;
    FParagraphStyleCollectionIndex: TdxIntegersDictionary;
    FParagraphStyleListOverrideIndexMap: TObjectDictionary<TdxParagraphStyle, TdxRtfNumberingListInfo>;
    FParameterValueString: TStringBuilder;
    FRtfLevels: TdxRtfListLevels;
    FSkipCount: Integer;
    FStateManager: TdxRichEditRtfParserStateManager;
    FStream: TStream;
    FTableStyleCollectionIndex: TdxIntegersDictionary;
    FLastParagraphFrameFormattingCacheIndex: Integer;
    FLastParagraphFramePropertiesCacheIndex: Integer;
    function ApplyCellIndices(var ACellIndices: TArray<Integer>;
      AStartColumnIndex, AColumnCount, ACellIndex: Integer): Integer;
    procedure ApplyFrameProperties(AParagraphIndex: TdxParagraphIndex);
    procedure ApplyParagraphFrameFormatting(AParagraph: TdxParagraph; AParagraphFrameFormatting: TdxParagraphFrameFormattingInfo); overload;
    procedure CheckTablesStructure;
    procedure CheckTableStructure(ATable: TdxTable);
    procedure CheckVerticalMerging(APrevRowCellIndices: TArray<Integer>; ATableRow: TdxTableRow);
    procedure CalculateRowCellIndices(var ACellIndices: TArray<Integer>; ARow: TdxTableRow);

    procedure ClearNumberingList;
    function GetStyleMergedTabs(ARtfStyleIndex: Integer): TdxTabFormattingInfo;
    function ReadByte(AStream: TStream; out AByte: Byte): Boolean; inline;
    function ReadChar(AStream: TStream; out AChar: Char): Boolean; inline;
    procedure SkipDataBeyondOuterBrace(AStream: TStream);

    procedure ParseBinChar(AChar: Char);
    procedure ParseChar(AChar: Char);
    procedure ParseHexChar(AStream: TStream; AChar: Char);

    function GetBookmarks: TdxImportBookmarkInfos;
    function GetRangePermissions: TdxImportRangePermissionInfos;
    function GetDestination: TdxRichEditRtfDestinationBase;
    function GetDocumentModel: TdxDocumentModel;
    function GetFields: TdxObjectStack<TdxRtfFieldInfo>;
    function GetPieceTable: TdxPieceTable;
    function GetPieceTableInfo: TdxRichEditRtfPieceTableInfo;
    function GetPosition: TdxRtfInputPosition;
    procedure SetDestination(const Value: TdxRichEditRtfDestinationBase);
    function GetOptions: TdxRtfDocumentImporterOptions;
    procedure SetBinCharCount(const Value: Integer);
  protected
    procedure AddNumberingListToParagraph(AParagraph: TdxParagraph; AListIndex: TdxNumberingListIndex; ALevelIndex: Integer);
    procedure ApplyParagraphFormattingCore(AParagraph: TdxParagraph; AParagraphFormatting: TdxRtfParagraphFormattingInfo);
    procedure ConvertListLevelsToAnotherType(AListIndex: TdxNumberingListIndex);
    procedure ConvertListLevelsToAnotherTypeCore(const ALevels: TdxListLevelCollection; const ADisplayFormat, AFontName: string; AFormat: TdxNumberingFormat);
    procedure DecreaseSkipCount;
    function GetTableReader: TdxRtfTableReader; virtual;
    procedure LinkParagraphStyleWithNumberingLists(AStyle: TdxParagraphStyle); virtual;
    procedure InitializeStateManager;
    procedure ImportCore(AStream: TStream); override;
    procedure ImportRtfStream(AStream: TStream); virtual;
    class function IsAlpha(C: Char): Boolean; static; inline;
    class function IsDigit(C: Char): Boolean; static; inline;
    procedure ParseCR;
    procedure ParseLF;
    function ParseRtfKeyword(AStream: TStream): Char;
    function PopRtfState: TdxRichEditPopRtfState; virtual;
    procedure PushRtfState;
    procedure TranslateControlChar(AChar: Char); virtual;
    procedure TranslateKeyword(const AKeyword: string; AParameterValue: Integer; AHasParameter: Boolean);
    function CheckSignature(AStream: TStream): Boolean;

    property OptionalGroupLevel: Integer read FOptionalGroupLevel write FOptionalGroupLevel;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel;
      const AOptions: IdxImporterOptions); override;
    destructor Destroy; override;

    class procedure ThrowInvalidRtfFile; static;
    class procedure ThrowInvalidFile; override;
    class procedure ThrowUnexpectedEndOfFile; static;

    procedure ApplyCharacterProperties(ACharacterProperties: TdxCharacterProperties;
      ACharacterFormattingInfo: TdxCharacterFormattingInfo; AParentCharacterFormatting: TdxMergedCharacterProperties;
      AResetUse: Boolean = True);
    procedure ApplyFormattingToLastParagraph;
    procedure ApplyLineSpacing(AParagraphFormatting: TdxRtfParagraphFormattingInfo);
    procedure ApplyParagraphFrameFormatting(AFrameProperties: TdxFrameProperties; AParagraphFrameFormatting: TdxParagraphFrameFormattingInfo); overload;
    procedure ApplyParagraphProperties(AParagraphProperties: TdxParagraphProperties;
      AParagraphFormatting: TdxParagraphFormattingInfo;
      AParentParagraphProperties: TdxMergedParagraphProperties;
      AResetUse: Boolean = True); overload;
    procedure ApplyParagraphProperties(AParagraphProperties: TdxParagraphProperties;
      AParentParagraphRtfStyleIndex: Integer;
      AParagraphFormatting: TdxRtfParagraphFormattingInfo); overload;
    procedure ApplyTableProperties(ATableProperties: TdxTableProperties; AParentTableProperties: TdxMergedTableProperties);
    procedure ApplyTableRowProperties(ARowProperties: TdxTableRowProperties; AParentRowProperties: TdxMergedTableRowProperties);
    procedure ApplyTableCellProperties(ACellProperties: TdxTableCellProperties; AParentCellProperties: TdxMergedTableCellProperties);
    procedure ApplyTabs(ATarget: TdxTabProperties; AParentParagraphRtfStyleIndex: Integer; AImportedTabs: TdxTabFormattingInfo);
    procedure ApplyParagraphFormatting(AParagraphIndex: TdxParagraphIndex; ASectionBreak: Boolean);
    procedure ApplySectionFormatting(ASkipNumbering: Boolean = False);
    procedure FlushDecoder;
    function GetUserNameById(AId: Integer): string; virtual;
    procedure LinkParagraphStylesWithNumberingLists; virtual;
    procedure ParseUnicodeChar(AChar: Char);
    procedure ParseCharWithoutDecoding(AChar: Char);
    procedure SetCodePage(ACodePage: Cardinal);
    procedure SetFont(AFontInfo: TdxRtfFontInfo);

    function GetCharacterStyleIndex(ARtfCharacterStyleIndex: Integer): Integer;
    function GetParagraphStyleIndex(ARtfParagraphStyleIndex: Integer): Integer;
    function GetStyleMergedCharacterProperties(ARtfStyleIndex: Integer): TdxMergedCharacterProperties; overload;
    function GetStyleMergedCharacterProperties(AParentMergedProperties: TdxMergedCharacterProperties): TdxMergedCharacterProperties; overload;
    function GetStyleMergedParagraphCharacterProperties(ARtfCharacterStyleIndex, ARtfParagraphStyleIndex: Integer): TdxMergedCharacterProperties; overload;
    function GetStyleMergedParagraphProperties(ARtfStyleIndex: Integer): TdxMergedParagraphProperties; overload;
    function GetStyleMergedParagraphProperties(AParentMergedProperties: TdxMergedParagraphProperties): TdxMergedParagraphProperties; overload;
    function GetStyleMergedTableRowProperties(AParentMergedProperties: TdxMergedTableRowProperties): TdxMergedTableRowProperties;
    function GetStyleMergedTableCellProperties(AParentMergedProperties: TdxMergedTableCellProperties): TdxMergedTableCellProperties;
    function GetTableStyleIndex(ARtfTableStyleIndex: Integer; AUnknowStyleIndex: Integer = 0): Integer;
    procedure InsertParagraph;
    procedure InsertSection;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property BinCharCount: Integer read FBinCharCount write SetBinCharCount;
    property Bookmarks: TdxImportBookmarkInfos read GetBookmarks;
    property RangePermissions: TdxImportRangePermissionInfos read GetRangePermissions;
    property CharacterStyleCollectionIndex: TdxIntegersDictionary read FCharacterStyleCollectionIndex;
    property Destination: TdxRichEditRtfDestinationBase read GetDestination write SetDestination;
    property DocumentProperties: TdxRtfDocumentProperties read FDocumentProperties;
    property Fields: TdxObjectStack<TdxRtfFieldInfo> read GetFields;
    property IsContainsParagraphFrame: Boolean read FIsContainsParagraphFrame write FIsContainsParagraphFrame;
    property LinkParagraphStyleIndexToCharacterStyleIndex: TdxIntegersDictionary read FLinkParagraphStyleIndexToCharacterStyleIndex;
    property ListOverrideIndexToNumberingListIndexMap: TDictionary<Integer, TdxNumberingListIndex> read FListOverrideIndexToNumberingListIndexMap;
    property NextParagraphStyleIndexTable: TdxIntegersDictionary read FNextParagraphStyleIndexTable;
    property NumberingListToOldListLevelInfoMap: TDictionary<TdxNumberingListIndex, TdxRtfOldListLevelInfo> read FNumberingListToOldListLevelInfoMap;
    property Options: TdxRtfDocumentImporterOptions read GetOptions;
    property ParagraphStyleCollectionIndex: TdxIntegersDictionary read FParagraphStyleCollectionIndex;
    property ParagraphStyleListOverrideIndexMap: TObjectDictionary<TdxParagraphStyle, TdxRtfNumberingListInfo> read FParagraphStyleListOverrideIndexMap;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property PieceTableInfo: TdxRichEditRtfPieceTableInfo read GetPieceTableInfo;
    property Position: TdxRtfInputPosition read GetPosition;
    property StateManager: TdxRichEditRtfParserStateManager read FStateManager;
    property TableReader: TdxRtfTableReader read GetTableReader;
    property TableStyleCollectionIndex: TdxIntegersDictionary read FTableStyleCollectionIndex;

    // for internal use
    property RtfLevels: TdxRtfListLevels read FRtfLevels;
  end;

  { TdxImportRtfFormat }

  TdxImportRtfFormat = class(TdxImportFileFormat)
  public
    class function GetDocumentFormat: TdxRichEditDocumentFormat; override;
    function GetImporter(ADocumentModel: TdxCustomDocumentModel;
      const AOptions: IdxImporterOptions): TdxDocumentModelImporter; override;
    function GetDocumentImporter: IdxImporter<TdxRichEditDocumentFormat, Boolean>; override;
  end;

function dxHexToInt(AChar: Char): Integer;

{.$UNDEF AQTIME}

implementation

uses
{$IFDEF AQTIME}
  AQtimeHelpers,
{$ENDIF}
  Windows, Math, Character,
  Contnrs, RTLConsts,
  cxClasses,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Import.Rtf.DestinationPieceTable,
  dxRichEdit.Import.Rtf.DestinationSkip,
  dxRichEdit.Import.Rtf.DestinationDefault,
  dxRichEdit.Import.Rtf.DestinationFontTable,
  dxRichEdit.Import.Rtf.DestinationColorTable,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Cache,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.Utils.Exceptions,
  dxCharacters,
  dxEncoding,
  dxStringHelper,
  dxRichEdit.Import.Rtf.DocumentImporter;

function dxHexToInt(AChar: Char): Integer;
begin
  if CharInSet(AChar, ['0'..'9']) then
    Result := Ord(AChar) - Ord('0')
  else
  begin
    if CharInSet(AChar, ['a'..'z']) then
      Result := 10 + Ord(AChar) - Ord('a')
    else
      if CharInSet(AChar, ['A'..'Z']) then
        Result := 10 + Ord(AChar) - Ord('A')
      else
        Result := 0;
  end;
end;

{ TdxRtfImageInfo }

constructor TdxRtfImageInfo.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
end;

procedure TdxRtfImageInfo.LoadMetafileFromStream(AStream: TBytesStream;
  AMapMode: TdxMapMode; APictureWidth, APictureHeight: Integer);
var
  AMetafile: TdxOfficeMetafile;
  ACalculator: TdxMetafilePhysicalDimensionCalculator;
  ASize: TSize;
begin
  CreateImage(TdxMetafileHelper.CreateMetafile(AStream, AMapMode, APictureWidth, APictureHeight));
  AMetafile := Safe<TdxOfficeMetafile>.Cast(FRtfImage.Image);
  if (AMetafile <> nil) and ((APictureWidth <= 0) or (APictureHeight <= 0)) then
  begin
    ACalculator := TdxMetafilePhysicalDimensionCalculator.Create;
    try
      ASize := ACalculator.Calculate(AMetafile, AStream.Bytes);
      if APictureWidth > 0 then
        ASize.cx := APictureWidth;
      if APictureHeight > 0 then
        ASize.cy := APictureHeight;
      AMetafile.MetafileSizeInHundredthsOfMillimeter := ASize;
    finally
      ACalculator.Free;
    end;
  end;
end;

procedure TdxRtfImageInfo.SetRtfImage(const Value: TdxOfficeImageReference);
begin
  FRtfImage.Free;
  FRtfImage := Value;
end;

procedure TdxRtfImageInfo.LoadImageFromStream(AStream: TBytesStream);
var
  AImage: TdxOfficeImage;
begin
  AImage := TdxOfficeImage.Create;
  try
    AImage.LoadFromStream(AStream);
  except
    FreeAndNil(AImage);
  end;
  if AImage.Empty then
    FreeAndNil(AImage);
  if AImage <> nil then
    CreateImage(AImage);
end;

procedure TdxRtfImageInfo.LoadDibFromStream(AStream: TBytesStream;
  AWidth, AHeight, ABytesInLine: Integer);
begin
  CreateImage(TdxDibHelper.CreateDib(AStream, AWidth, AHeight, ABytesInLine));
end;

procedure TdxRtfImageInfo.CreateImage(ANativeImage: TdxOfficeImage);
begin
  RtfImage := TdxOfficeImageReference.Create(DocumentModel.ImageCache, ANativeImage);
end;

destructor TdxRtfImageInfo.Destroy;
begin
  FreeAndNil(FRtfImage);
  inherited Destroy;
end;

procedure TdxRtfImageInfo.LoadBitmapFromStream(AStream: TBytesStream;
  AWidth, AHeight, AColorPlanesCount, ABitsPerPixel, ABytesInLine: Integer);
begin
  CreateImage(TdxBitmapHelper.CreateBitmap(AStream, AWidth, AHeight,
    AColorPlanesCount, ABitsPerPixel, ABytesInLine));
end;

{ TdxRtfFontInfo }

constructor TdxRtfFontInfo.Create;
begin
  inherited Create;
  FCharset := -1;
end;

{ TdxRtfFontInfoCollection }

constructor TdxRtfFontInfoCollection.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create(True);
  FDefaultRtfFontInfo := CreateDefaultRtfFontInfo(ADocumentModel);
end;

function TdxRtfFontInfoCollection.GetRtfFontInfoById(AID: Integer): TdxRtfFontInfo;
var
  I: Integer;
begin
  Result := FDefaultRtfFontInfo;
  for I := Count - 1 downto 0 do
  begin
    if Items[I].ID = AID then
    begin
      Result := Items[I];
      Break;
    end;
  end;

end;

function TdxRtfFontInfoCollection.CreateDefaultRtfFontInfo(ADocumentModel: TdxDocumentModel): TdxRtfFontInfo;
begin
  Result := TdxRtfFontInfo.Create;
  Result.Name := 'Times New Roman';
  Result.ID := MaxInt;
end;

destructor TdxRtfFontInfoCollection.Destroy;
begin
  FreeAndNil(FDefaultRtfFontInfo);
  inherited Destroy;
end;

function TdxRtfFontInfoCollection.GetItem(Index: Integer): TdxRtfFontInfo;
begin
  Result := TdxRtfFontInfo(inherited Items[Index]);
end;

{ TdxRichEditRtfDestinationBase }

constructor TdxRichEditRtfDestinationBase.Create(AImporter: TdxRtfImporter);
begin
  inherited Create;
  FImporter := AImporter;
  FPieceTable := Importer.PieceTable;
  Importer.Position.RtfFormattingInfo := Importer.Position.RtfFormattingInfo.Clone;
end;

destructor TdxRichEditRtfDestinationBase.Destroy;
begin
  inherited Destroy;
end;

class constructor TdxRichEditRtfDestinationBase.Initialize;
begin
  FDefaultKeywordHT := CreateDefaultKeywordHT;
  FDefaultControlCharHT := CreateDefaultControlCharHT;
end;

class destructor TdxRichEditRtfDestinationBase.Finalize;
begin
  FreeAndNil(FDefaultControlCharHT);
  FreeAndNil(FDefaultKeywordHT);
end;

procedure TdxRichEditRtfDestinationBase.ProcessBinChar(AChar: Char);
begin
  ProcessBinCharCore(AChar);
  FNonEmpty := True;
end;

procedure TdxRichEditRtfDestinationBase.ProcessChar(AChar: Char);
begin
  ProcessCharCore(AChar);
  FNonEmpty := True;
end;

procedure TdxRichEditRtfDestinationBase.ProcessControlChar(AChar: Char);
begin
  ProcessControlCharCore(AChar);
  FNonEmpty := True;
end;

function TdxRichEditRtfDestinationBase.ProcessKeyword(const AKeyword: string; AParameterValue: Integer; AHasParameter: Boolean): Boolean;
begin
  Result := ProcessKeywordCore(AKeyword, AParameterValue, AHasParameter);
  FNonEmpty := True;
end;

procedure TdxRichEditRtfDestinationBase.ProcessText(const AText: string);
begin
  ProcessTextCore(AText);
  FNonEmpty := True;
end;

procedure TdxRichEditRtfDestinationBase.AfterPopRtfState;
begin
//do nothing
end;

procedure TdxRichEditRtfDestinationBase.BeforePopRtfState;
begin
//do nothing
end;

procedure TdxRichEditRtfDestinationBase.BeforeNestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase);
begin
  BeforeNestedGroupFinishedCore(ADestination);
  FNonEmpty := True;
end;

procedure TdxRichEditRtfDestinationBase.NestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase);
begin
//do nothing
end;

function TdxRichEditRtfDestinationBase.CanAppendText: Boolean;
begin
  Result := False;
end;

function TdxRichEditRtfDestinationBase.CanProcessDefaultKeyword: Boolean;
begin
  Result := True;
end;

function TdxRichEditRtfDestinationBase.GetPieceTable: TdxPieceTable;
begin
  Result := FPieceTable;
end;

procedure TdxRichEditRtfDestinationBase.IncreaseGroupLevel;
begin
//do nothing
end;

procedure TdxRichEditRtfDestinationBase.ProcessBinCharCore(AChar: Char);
begin
//do nothing
end;

procedure TdxRichEditRtfDestinationBase.ProcessCharCore(AChar: Char);
begin
end;

procedure TdxRichEditRtfDestinationBase.ProcessControlCharCore(AChar: Char);
var
  ATranslator: TdxTranslateControlCharEvent;
begin
  if ((ControlCharHT <> nil) and ControlCharHT.TryGetValue(AChar, ATranslator)) or
     DefaultControlCharHT.TryGetValue(AChar, ATranslator) then
    ATranslator(Importer, AChar);
end;

function TdxRichEditRtfDestinationBase.ProcessKeywordCore(const AKeyword: string; AParameterValue: Integer; AHasParameter: Boolean): Boolean;
var
  ATranslator: TdxTranslateKeywordEvent;
begin
  ATranslator := nil;
  if KeywordHT <> nil then
    KeywordHT.TryGetValue(AKeyword, ATranslator);

  if not Assigned(ATranslator) then
    FDefaultKeywordHT.TryGetValue(AKeyword, ATranslator);

  Result := Assigned(ATranslator);
  if Result then
    ATranslator(Importer, AParameterValue, AHasParameter);
end;

procedure TdxRichEditRtfDestinationBase.ProcessTextCore(const AText: string);
begin
  Assert(CanAppendText);
end;

procedure TdxRichEditRtfDestinationBase.BeforeNestedGroupFinishedCore(ADestination: TdxRichEditRtfDestinationBase);
begin
end;

class procedure TdxRichEditRtfDestinationBase.BinKeywordHandler(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter and (AParameterValue <> 0) then
  begin
    AImporter.BinCharCount := AParameterValue;
    AImporter.StateManager.ParsingState := TdxRichEditRtfParsingState.BinData;
  end
  else
    AImporter.DecreaseSkipCount;
end;

class procedure TdxRichEditRtfDestinationBase.UnicodeKeywordHandler(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean);
var
  AChar: Char;
begin
  AChar := Char(AParameterValue and $FFFF);
  if (Ord(AChar) >= $F020) and (Ord(AChar) <= $F0FF) then
    AChar := Char(Ord(AChar) - $F000);
  AImporter.ParseUnicodeChar(AChar);
end;

class procedure TdxRichEditRtfDestinationBase.ColorTableKeywordHandler(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxColorTableDestination.Create(AImporter);
end;

class procedure TdxRichEditRtfDestinationBase.FontTableKeywordHandler(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxFontTableDestination.Create(AImporter);
end;

class procedure TdxRichEditRtfDestinationBase.UserTableKeywordHandler(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxUserTableDestination.Create(AImporter);
end;

class procedure TdxRichEditRtfDestinationBase.AnsiKeywordHandler(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.SetCodePage(TdxEncoding.GetEncodingCodePage(TdxRtfDocumentImporterOptions.DefaultEncoding));
end;

class procedure TdxRichEditRtfDestinationBase.MacKeywordHandler(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultCodePage := macCodePage;
  AImporter.SetCodePage(macCodePage);
end;

class procedure TdxRichEditRtfDestinationBase.PcKeywordHandler(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultCodePage := pcCodePage;
  AImporter.SetCodePage(pcCodePage);
end;

class procedure TdxRichEditRtfDestinationBase.PcaKeywordHandler(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.DocumentProperties.DefaultCodePage := pcaCodePage;
  AImporter.SetCodePage(pcaCodePage);
end;

class procedure TdxRichEditRtfDestinationBase.AnsiCodePageKeywordHandler(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean);
begin
  if AHasParameter then
  begin
    AImporter.DocumentProperties.DefaultCodePage := AParameterValue;
    AImporter.SetCodePage(AParameterValue);
  end;
end;

class procedure TdxRichEditRtfDestinationBase.HyphAutoKeywordHandler(AImporter: TdxRtfImporter;
  AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AHasParameter then
    AParameterValue := 1;
  AImporter.DocumentModel.DocumentProperties.HyphenateDocument := AParameterValue <> 0;
end;

class procedure TdxRichEditRtfDestinationBase.EscapedCharHandler(AImporter: TdxRtfImporter; var AChar: Char);
begin
  AImporter.FlushDecoder;
  AImporter.Destination.ProcessChar(AChar);
end;

class procedure TdxRichEditRtfDestinationBase.SwitchToHexCharHandler(AImporter: TdxRtfImporter;
  var AChar: Char);
begin
  AImporter.StateManager.ParsingState := TdxRichEditRtfParsingState.HexData;
end;

class procedure TdxRichEditRtfDestinationBase.OptionalGroupCharHandler(AImporter: TdxRtfImporter;
  var AChar: Char);
begin
  if AImporter.Destination.NonEmpty then
    Exit;
  AImporter.FlushDecoder;
  AImporter.OptionalGroupLevel := AImporter.StateManager.States.Count;
end;

class function TdxRichEditRtfDestinationBase.CreateDefaultControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := TdxControlCharTranslatorTable.Create;
  Result.Add('''', SwitchToHexCharHandler);
  Result.Add('*', OptionalGroupCharHandler);
end;

class function TdxRichEditRtfDestinationBase.CreateDefaultKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('bin', BinKeywordHandler);
  Result.Add('colortbl', ColorTableKeywordHandler);
  Result.Add('fonttbl', FontTableKeywordHandler);
  Result.Add('protusertbl', UserTableKeywordHandler);
  Result.Add('ansi', AnsiKeywordHandler);
  Result.Add('mac', MacKeywordHandler);
  Result.Add('pc', PcKeywordHandler);
  Result.Add('pca', PcaKeywordHandler);
  Result.Add('ansicpg', AnsiCodePageKeywordHandler);
  Result.Add('hyphauto', HyphAutoKeywordHandler);
end;

class function TdxRichEditRtfDestinationBase.GetControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := nil;
end;

class function TdxRichEditRtfDestinationBase.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := nil;
end;

{ TdxRichEditRtfPieceTableInfo }

constructor TdxRichEditRtfPieceTableInfo.Create(AImporter: TdxRtfImporter;
  APieceTable: TdxPieceTable);
begin
  inherited Create(APieceTable);
  FCharacterFormattingOptions := TdxCharacterFormattingOptions.Create(TdxCharacterFormattingOptions.MaskUseAll);
  FPosition := TdxRtfInputPosition.Create(PieceTable, AImporter);
  FPosition.CharacterFormatting.ReplaceInfo(APieceTable.DocumentModel.Cache.CharacterFormattingInfoCache.DefaultItem, FCharacterFormattingOptions);
  FPosition.CharacterFormatting.BeginUpdate;
  FPosition.CharacterFormatting.DoubleFontSize := 24;
  FPosition.CharacterFormatting.FontName := 'Times New Roman';
  FPosition.CharacterFormatting.EndUpdate;
  FTableReader := TdxRtfTableReader.Create(AImporter);
  FFields := TdxObjectStack<TdxRtfFieldInfo>.Create(True);
  FParagraphTableStyles := TdxIntegersDictionary.Create;
end;

destructor TdxRichEditRtfPieceTableInfo.Destroy;
begin
  FreeAndNil(FParagraphTableStyles);
  FreeAndNil(FFields);
  FreeAndNil(FTableReader);
  FreeAndNil(FPosition);
  inherited Destroy;
end;

{ TdxCharacterDecoder }

constructor TdxCharacterDecoder.Create(ACodePage: Cardinal);
begin
  inherited Create;
  FEncoding := TdxEncoding.GetEncoding(ACodePage and $FFFF)
end;

{ TdxCodePageCharacterDecoder }

constructor TdxCodePageCharacterDecoder.Create(ACodePage: Cardinal);
begin
  inherited Create(ACodePage);
  FCapacity := 4096;
  SetLength(FBytes, FCapacity);
end;

destructor TdxCodePageCharacterDecoder.Destroy;
begin
  FCapacity := 0;
  SetLength(FBytes, 0);
  cxClearObjectLinks(Self);
  inherited Destroy;
end;

procedure TdxCodePageCharacterDecoder.ProcessChar(AImporter: TdxRtfImporter; AChar: Char);
begin
  if FCount = FCapacity then
    Flush(AImporter);
  AddByte(Byte(AChar));
end;

procedure TdxCodePageCharacterDecoder.Flush(AImporter: TdxRtfImporter);
var
  ACount: Integer;
  AChars: TCharArray;
  ALink: TcxObjectLink;
begin
  if FCount > 0 then
  begin
    ALink := cxAddObjectLink(Self);
    try
      AChars := Encoding.GetChars(FBytes, 0, FCount);
      try
        ACount := Length(AChars);
        if not AImporter.Destination.CanAppendText or (ACount <= 1) then
          FlushByChar(AImporter, AChars)
        else
          FlushByString(AImporter, AChars);
      finally
        SetLength(AChars, 0);
      end;
      if ALink.Ref <> nil then
        Clear;
    finally
      cxRemoveObjectLink(ALink);
    end;
  end;
end;

procedure TdxCodePageCharacterDecoder.AddByte(Value: Byte);
begin
  if FCount >= FCapacity then
  begin
    Inc(FCapacity, 4096);
    SetLength(FBytes, FCapacity);
  end;
  FBytes[FCount] := Value;
  Inc(FCount);
end;

procedure TdxCodePageCharacterDecoder.Clear;
begin
  FCount := 0;
  FCapacity := 0;
  SetLength(FBytes, FCapacity);
end;

procedure TdxCodePageCharacterDecoder.FlushByChar(AImporter: TdxRtfImporter; const AChars: TCharArray);
var
  I: Integer;
  ADestination: TdxRichEditRtfDestinationBase;
begin
  ADestination := AImporter.Destination;
  for I := Low(AChars) to High(AChars) do
    ADestination.ProcessChar(AChars[I]);
end;

procedure TdxCodePageCharacterDecoder.FlushByString(AImporter: TdxRtfImporter; const AChars: TCharArray);
var
  S: string;
begin
  SetString(S, PChar(@AChars[0]), Length(AChars));
  AImporter.Destination.ProcessText(S);
end;

{ TdxEmptyCharacterDecoder }

constructor TdxEmptyCharacterDecoder.Create;
begin
  inherited Create(TdxEncoding.GetEncodingCodePage(TdxRtfDocumentImporterOptions.DefaultEncoding));
end;

procedure TdxEmptyCharacterDecoder.Flush(
  AImporter: TdxRtfImporter);
begin
//do nothing
end;

procedure TdxEmptyCharacterDecoder.ProcessChar(AImporter: TdxRtfImporter; AChar: Char);
begin
  AImporter.Destination.ProcessChar(AChar);
end;

{ TdxRtfFormattingInfo }

constructor TdxRtfFormattingInfo.Create(ACodePage: Cardinal);
begin
  inherited Create;
  FUnicodeCharacterByteCount := 1;
  FParentStyleIndex := -1;
  SetCodePageCore(ACodePage);
end;

destructor TdxRtfFormattingInfo.Destroy;
begin
  FreeAndNil(FDecoder);
  inherited Destroy;
end;

procedure TdxRtfFormattingInfo.CopyFrom(const Source: TdxRtfFormattingInfo);
begin
  FUnicodeCharacterByteCount := Source.UnicodeCharacterByteCount;
  CodePage := Source.CodePage;
  ParentStyleIndex := Source.ParentStyleIndex;
  Deleted := Source.Deleted;
end;

function TdxRtfFormattingInfo.Clone: TdxRtfFormattingInfo;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxRtfFormattingInfoClass(ClassType).Create(CodePage);
  Result.CopyFrom(Self);
end;

function TdxRtfFormattingInfo.ChooseDecoder: TdxCodePageCharacterDecoder;
begin
  Result := TdxCodePageCharacterDecoder.Create(CodePage);
end;

procedure TdxRtfFormattingInfo.SetDecoder(Value: TdxCodePageCharacterDecoder; ANeedDestroyOldDecoder: Boolean = True);
begin
  if Decoder <> Value then
  begin
    Assert(Value <> nil);
    if ANeedDestroyOldDecoder then
      FDecoder.Free;
    FDecoder := Value;
  end;
end;

procedure TdxRtfFormattingInfo.SetCodePageCore(Value: Cardinal);
begin
  FCodePage := Value;
  SetDecoder(ChooseDecoder)
end;

procedure TdxRtfFormattingInfo.SetCodePage(Value: Cardinal);
begin
  if FCodePage <> Value then
    SetCodePageCore(Value);
end;

procedure TdxRtfFormattingInfo.SetUnicodeCharacterByteCount(Value: Integer);
begin
  if Value < 0 then
    TdxRtfImporter.ThrowInvalidRtfFile;
  FUnicodeCharacterByteCount := value;
end;

{ TdxRtfOldListLevelInfo }

constructor TdxRtfOldListLevelInfo.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
  FCharacterProperties := ADocumentModel.CreateEmptyCharacterFormatting;
  FCharacterProperties.BeginUpdate;
  FCharacterProperties.FontName := 'Times New Roman';
  FCharacterProperties.DoubleFontSize := 24;
  FCharacterProperties.ResetUse(TdxCharacterFormattingOptions.MaskUseAll);
  FCharacterProperties.EndUpdate;
  FListLevelProperties := TdxListLevelInfo.Create;
end;

destructor TdxRtfOldListLevelInfo.Destroy;
begin
  FreeAndNil(FCharacterProperties);
  FreeAndNil(FListLevelProperties);
  inherited Destroy;
end;

procedure TdxRtfOldListLevelInfo.CopyFrom(const Source: TdxRtfOldListLevelInfo);
begin
  if Source is TdxRtfOldListLevelInfo then
  begin
    CharacterProperties.CopyFrom(Source.CharacterProperties);
    IncludeInformationFromPreviousLevel := Source.IncludeInformationFromPreviousLevel;
    Indent := Source.Indent;
    ListLevelProperties.CopyFrom(Source.ListLevelProperties);
    SkipNumbering := Source.SkipNumbering;
    TextAfter := Source.TextAfter;
    TextBefore := Source.TextBefore;
  end;
end;

function TdxRtfOldListLevelInfo.Clone: TdxRtfOldListLevelInfo;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxRtfOldListLevelInfoClass(ClassType).Create(FDocumentModel);
  Result.CopyFrom(Self);
end;

{ TdxRtfOldListLevelInfoCollection }

constructor TdxRtfOldListLevelInfoCollection.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
end;

procedure TdxRtfOldListLevelInfoCollection.EnsureLevelIndex(Index: Integer);
var
  ACount, I: Integer;
begin
  ACount := Count;
  for I := ACount to Index do
    Add(TdxRtfOldListLevelInfo.Create(FDocumentModel));
end;

function TdxRtfOldListLevelInfoCollection.Clone: TdxRtfOldListLevelInfoCollection;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxRtfOldListLevelInfoCollectionClass(ClassType).Create(FDocumentModel);
  Result.CopyFrom(Self);
end;

procedure TdxRtfOldListLevelInfoCollection.CopyFrom(ASource: TdxRtfOldListLevelInfoCollection);
var
  I: Integer;
begin
  Clear;
  for I := 0 to ASource.Count - 1 do
    Add(ASource.Items[I].Clone);
end;

function TdxRtfOldListLevelInfoCollection.GetItem(Index: Integer): TdxRtfOldListLevelInfo;
begin
  if Count <= Index then
    EnsureLevelIndex(Index);
  Result := TdxRtfOldListLevelInfo(inherited Items[Index]);
end;

{ TdxRtfInputPositionState }

constructor TdxRtfInputPositionState.Create(APosition: TdxRtfInputPosition);
begin
  inherited Create;
  FCharacterFormatting := TdxCharacterFormattingBase(APosition.CharacterFormatting.Clone);
  FCharacterStyleIndex := APosition.CharacterStyleIndex;
  FParagraphFormattingInfo := TdxRtfParagraphFormattingInfo(APosition.ParagraphFormattingInfo.Clone);
  FRtfFormattingInfo := APosition.RtfFormattingInfo.Clone;
  TdxReferencedObject.AddReference(FRtfFormattingInfo);
  FParagraphFrameFormattingInfo := TdxParagraphFrameFormattingInfo(APosition.ParagraphFrameFormattingInfo.Clone);
  FCurrentOldMultiLevelListIndex := APosition.CurrentOldMultiLevelListIndex;
  FCurrentOldSimpleListIndex := APosition.CurrentOldSimpleListIndex;
  FCurrentOldSimpleList := APosition.CurrentOldSimpleList;
  FCurrentOldListLevelNumber := APosition.CurrentOldListLevelNumber;
  FCurrentOldListSkipNumbering := APosition.CurrentOldListSkipNumbering;
  FTableStyleIndex := APosition.TableStyleIndex;
  FFontType := APosition.FontType;
  FDoubleByteCharactersFontName := APosition.DoubleByteCharactersFontName;
  FOldListLevelInfo := APosition.OldListLevelInfo.Clone;
  FOldListLevelInfoCollection := APosition.OldListLevelInfoCollection;
  FLowAnsiCharactersFontName := APosition.LowAnsiCharactersFontName;
  FHighAnsiCharactersFontName := APosition.HighAnsiCharactersFontName;
end;

destructor TdxRtfInputPositionState.Destroy;
begin
  FreeAndNil(FParagraphFrameFormattingInfo);
  FreeAndNil(FOldListLevelInfo);
  TdxReferencedObject.Release(FRtfFormattingInfo);
  FreeAndNil(FParagraphFormattingInfo);
  FreeAndNil(FCharacterFormatting);
  inherited Destroy;
end;

{ TdxRtfInputPosition }

constructor TdxRtfInputPosition.Create(APieceTable: TdxSimplePieceTable; AImporter: TdxRtfImporter);
begin
  inherited Create(APieceTable);
  FRtfFormattingInfo := TdxRtfFormattingInfo.Create(TdxEncoding.GetEncodingCodePage(TdxRtfDocumentImporterOptions.DefaultEncoding));
  TdxReferencedObject.AddReference(FRtfFormattingInfo);
  FFieldInfo := TdxRtfFieldInfo.Create;
  CharacterFormatting.BeginUpdate;
  CharacterFormatting.FontName := 'Times New Roman';
  CharacterFormatting.DoubleFontSize := 24;
  CharacterFormatting.ResetUse(TdxCharacterFormattingOptions.MaskUseAll);
  CharacterFormatting.EndUpdate;

  FParagraphFormattingInfo := TdxRtfParagraphFormattingInfo.Create;
  FSectionFormattingInfo := TdxRtfSectionFormattingInfo.Create(TdxDocumentModel(APieceTable.DocumentModel));
  SectionFormattingInfo.InitializeDefault(TdxDocumentModel(APieceTable.DocumentModel));

  FCurrentOldMultiLevelListIndex := NumberingListIndexListIndexNotSetted;
  FCurrentOldSimpleListIndex := NumberingListIndexListIndexNotSetted;
end;

destructor TdxRtfInputPosition.Destroy;
begin
  if FIsOldListLevelInfoCollectionOwner then
    FreeAndNil(FOldListLevelInfoCollection);
  FreeAndNil(FFieldInfo);
  FreeAndNil(FOldListLevelInfo);
  FreeAndNil(FSectionFormattingInfo);
  FreeAndNil(FParagraphFormattingInfo);
  TdxReferencedObject.Release(FRtfFormattingInfo);
  FreeAndNil(FParagraphFrameFormattingInfo);
  inherited Destroy;
end;

function TdxRtfInputPosition.GetState: TdxRtfInputPositionState;
begin
  Result := TdxRtfInputPositionState.Create(Self);
end;

procedure TdxRtfInputPosition.RecalcUseAssociatedProperties;
begin
  FUseHighAnsiCharactersFontName := FHighAnsiCharactersFontName <> '';
  FUseLowAnsiCharactersFontName := FLowAnsiCharactersFontName <> '';
  FUseDoubleByteCharactersFontName := FDoubleByteCharactersFontName <> '';
end;

procedure TdxRtfInputPosition.SetFont(const AFontName: string);
begin
  if FontType = TdxRtfFontType.Undefined then
  begin
    CharacterFormatting.FontName := AFontName;
    ResetUseAssociatedProperties;
  end
  else
  begin
    if FontType = TdxRtfFontType.DoubleByteCharactersFont then
      DoubleByteCharactersFontName := AFontName
    else
      if FontType = TdxRtfFontType.HighAnsiCharactersFont then
        HighAnsiCharactersFontName := AFontName
      else
        if FontType = TdxRtfFontType.LowAnsiCharactersFont then
          LowAnsiCharactersFontName := AFontName;
    RecalcUseAssociatedProperties;
    FontType := TdxRtfFontType.Undefined;
  end;
end;

function TdxRtfInputPosition.GetOldListLevelInfoCollection: TdxRtfOldListLevelInfoCollection;
begin
  if FOldListLevelInfoCollection = nil then
  begin
    FOldListLevelInfoCollection := TdxRtfOldListLevelInfoCollection.Create(TdxDocumentModel(PieceTable.DocumentModel));
    FIsOldListLevelInfoCollectionOwner := True;
  end;
  Result := FOldListLevelInfoCollection;
end;

procedure TdxRtfInputPosition.SetOldListLevelInfo(
  const Value: TdxRtfOldListLevelInfo);
begin
  FOldListLevelInfo.Free;
  FOldListLevelInfo := Value;
end;

procedure TdxRtfInputPosition.SetOldListLevelInfoCollection(const Value: TdxRtfOldListLevelInfoCollection);
begin
  if FOldListLevelInfoCollection = Value then
    Exit;
  if FIsOldListLevelInfoCollectionOwner then
    FOldListLevelInfoCollection.Free;
  FOldListLevelInfoCollection := Value;
  FIsOldListLevelInfoCollectionOwner := False;
end;

procedure TdxRtfInputPosition.ResetUseAssociatedProperties;
begin
  FUseHighAnsiCharactersFontName := False;
  FUseLowAnsiCharactersFontName := False;
  FUseDoubleByteCharactersFontName := False;
end;

procedure TdxRtfInputPosition.SetState(AState: TdxRtfInputPositionState);
begin
  CharacterFormatting.CopyFrom(AState.CharacterFormatting);
  CharacterStyleIndex := AState.CharacterStyleIndex;
  ParagraphFormattingInfo.CopyFrom(AState.ParagraphFormattingInfo);
  ParagraphFrameFormattingInfo := AState.ParagraphFrameFormattingInfo.Clone;
  RtfFormattingInfo := AState.RtfFormattingInfo;
  CurrentOldMultiLevelListIndex := AState.CurrentOldMultiLevelListIndex;
  CurrentOldSimpleListIndex := AState.CurrentOldSimpleListIndex;
  CurrentOldListLevelNumber := AState.CurrentOldListLevelNumber;
  CurrentOldListSkipNumbering := AState.CurrentOldListSkipNumbering;
  CurrentOldSimpleList := AState.CurrentOldSimpleList;
  TableStyleIndex := AState.TableStyleIndex;
  DoubleByteCharactersFontName := AState.DoubleByteCharactersFontName;
  if AState.OldListLevelInfo <> nil then
    OldListLevelInfo.CopyFrom(AState.OldListLevelInfo)
  else
    FreeAndNil(FOldListLevelInfo);
  SetOldListLevelInfoCollection(AState.OldListLevelInfoCollection);
  LowAnsiCharactersFontName := AState.LowAnsiCharactersFontName;
  HighAnsiCharactersFontName := AState.HighAnsiCharactersFontName;
  FontType := AState.FontType;
  RecalcUseAssociatedProperties;
end;

procedure TdxRtfInputPosition.SetParagraphFormattingInfo(const Value: TdxRtfParagraphFormattingInfo);
begin
  ParagraphFormattingInfo.CopyFrom(Value);
end;

procedure TdxRtfInputPosition.SetParagraphFrameFormattingInfo(const Value: TdxParagraphFrameFormattingInfo);
begin
  if FParagraphFrameFormattingInfo = Value then
    Exit;
  FParagraphFrameFormattingInfo.Free;
  FParagraphFrameFormattingInfo := Value;
end;

procedure TdxRtfInputPosition.SetRtfFormattingInfo(const Value: TdxRtfFormattingInfo);
begin
  TdxReferencedObject.Release(FRtfFormattingInfo);
  FRtfFormattingInfo := Value;
  TdxReferencedObject.AddReference(FRtfFormattingInfo);
end;

{ TdxRichEditRtfParserStateItem }

constructor TdxRichEditRtfParserStateItem.Create(AInputPositionState: TdxRtfInputPositionState; ADestination: TdxRichEditRtfDestinationBase);
begin
  inherited Create;
  FDestination := ADestination;
  FInputPositionState := AInputPositionState;
end;

{ TdxRichEditRtfParserStateManager }

constructor TdxRichEditRtfParserStateManager.Create(AImporter: TdxRtfImporter);
begin
  inherited Create;
  FImporter := AImporter;
  FStates := TdxRichEditRtfParserStateItems.Create(False);
  FPieceTables := TdxRichEditRtfPieceTableInfos.Create(False);
  FSuperfluousDestinations := TdxFastObjectList.Create;
end;

destructor TdxRichEditRtfParserStateManager.Destroy;
begin
  if FSuperfluousDestinations.IndexOf(FDestination) = -1 then
    FSuperfluousDestinations.Add(FDestination);
  DestroyPieceTables;
  DestroyStates;
  FreeAndNil(FSuperfluousDestinations);
  inherited Destroy;
end;

procedure TdxRichEditRtfParserStateManager.PopState;
var
  I: Integer;
  ANestedDestination, ANewDestination: TdxRichEditRtfDestinationBase;
  AState: TdxRichEditRtfParserStateItem;
  ASamePieceTable: Boolean;
  APieceTableInfo: TdxRichEditRtfPieceTableInfo;
  APieceTableInfos: TArray<TdxRichEditRtfPieceTableInfo>;
begin
  ANestedDestination := Destination;

  AState := States.Peek;
  ANewDestination := AState.Destination;
  ANewDestination.BeforeNestedGroupFinished(ANestedDestination);
  Destination.BeforePopRtfState;
  AState := States.Extract;
  try
    ASamePieceTable := Destination.PieceTable = ANewDestination.PieceTable;
    APieceTableInfo := PieceTables.Extract;
    SetDestinationCore(ANewDestination, False);
    PieceTables.Push(APieceTableInfo);

    Destination.NestedGroupFinished(ANestedDestination);
    if (AState.InputPositionState <> nil) and ASamePieceTable then
      Importer.Position.SetState(AState.InputPositionState);
  finally
    AState.InputPositionState.Free;
    AState.Free;
  end;
  PieceTables.Pop;

  if PieceTables.Count <= 0 then
    PieceTables.Push(APieceTableInfo)
  else
  begin
    APieceTableInfos := PieceTables.ToArray;
    for I := Length(APieceTableInfos) - 1 downto 0 do
      if APieceTableInfo = APieceTableInfos[I] then
      begin
        APieceTableInfo := nil;
        Break;
      end;
    APieceTableInfo.Free;
  end;
end;

procedure TdxRichEditRtfParserStateManager.PushState;
begin
  FStates.Push(TdxRichEditRtfParserStateItem.Create(Importer.Position.GetState, Destination));
  PieceTables.Push(PieceTableInfo);
  SetDestinationCore(Destination.CreateClone, True);
  Destination.IncreaseGroupLevel;
end;

procedure TdxRichEditRtfParserStateManager.CheckSuperfluousDestinations;
begin
  FSuperfluousDestinations.Clear;
end;

procedure TdxRichEditRtfParserStateManager.DestroyPieceTables;
var
  APieceTables: TdxObjectList<TdxRichEditRtfPieceTableInfo>;
  APieceTable: TdxRichEditRtfPieceTableInfo;
begin
  APieceTables := TdxObjectList<TdxRichEditRtfPieceTableInfo>.Create;
  try
    while FPieceTables.Count > 0 do
    begin
      APieceTable := FPieceTables.Extract;
      if APieceTables.IndexOf(APieceTable) = -1 then
        APieceTables.Add(APieceTable);
    end;
    FreeAndNil(FPieceTables);
  finally
    APieceTables.Free;
  end;
end;

procedure TdxRichEditRtfParserStateManager.DestroyStates;
var
  AState: TdxRichEditRtfParserStateItem;
begin
  while FStates.Count > 0 do
  begin
    AState := FStates.Extract;
    AState.InputPositionState.Free;
    if FSuperfluousDestinations.IndexOf(AState.Destination) = -1 then
      FSuperfluousDestinations.Add(AState.Destination);
    AState.Free;
  end;
  FreeAndNil(FStates);
end;

function TdxRichEditRtfParserStateManager.CreateDefaultDestination: TdxRichEditRtfDestinationBase;
begin
  Result := TdxDefaultDestination.Create(Importer, Importer.PieceTable);
end;

procedure TdxRichEditRtfParserStateManager.Initialize;
begin
  FPieceTables.Push(TdxRichEditRtfPieceTableInfo.Create(Importer, Importer.DocumentModel.MainPieceTable));
  FDestination := CreateDefaultDestination;
  FStates.Push(TdxRichEditRtfParserStateItem.Create(nil, Destination));
end;

procedure TdxRichEditRtfParserStateManager.SetDestinationCore(Value: TdxRichEditRtfDestinationBase; AUpdatePieceTableInfo: Boolean);
begin
  if (Value.PieceTable <> Destination.PieceTable) and AUpdatePieceTableInfo and (PieceTables.Count > 0) then
  begin
    PieceTables.Pop;
    PieceTables.Push(TdxRichEditRtfPieceTableInfo.Create(Importer, Value.PieceTable));
  end;
  FDestination := Value;
end;

function TdxRichEditRtfParserStateManager.GetPieceTableInfo: TdxRichEditRtfPieceTableInfo;
begin
  Result := TdxRichEditRtfPieceTableInfo(PieceTables.Peek);
end;

procedure TdxRichEditRtfParserStateManager.SetDestination(const Value: TdxRichEditRtfDestinationBase);
begin
  if Destination <> Value then
  begin
    if Destination <> nil then
      FSuperfluousDestinations.Add(Destination);
    SetDestinationCore(Value, True);
  end;
end;

{ TdxListLevelDisplayTextHelper }

class function TdxListLevelDisplayTextHelper.CreateDisplayFormatStringCore(APlaceholderIndices: TdxIntegerList; const AText: string): string;
var
  ASb: TStringBuilder;
  I, ACount: Integer;
begin
  ASb := TStringBuilder.Create;
  try
    ASb.Append(GetTextRange(APlaceholderIndices, 0, AText));
    ACount := APlaceholderIndices.Count - 1;
    for I := 1 to ACount - 1 do
    begin
      ASb.Append('%');
      ASb.Append(Ord(AText[APlaceholderIndices[I] + 1]));
      ASb.Append(':s');
      ASb.Append(GetTextRange(APlaceholderIndices, I, AText));
    end;
    Result := ASb.ToString;
  finally
    ASb.Free;
  end;
end;

class function TdxListLevelDisplayTextHelper.GetTextRange(APlaceholderIndices: TdxIntegerList; AStartPlaceHolderIndex: Integer; const AText: string): string;
var
  AIndex: Integer;
begin
  AIndex := APlaceholderIndices[AStartPlaceHolderIndex] + 1;
  Result := TdxStringHelper.Substring(AText, AIndex, APlaceholderIndices[AStartPlaceHolderIndex + 1] - AIndex);
  Result := StringReplace(Result, '%', '%%', [rfReplaceAll]);
end;

{ TdxRtfListLevel }

constructor TdxRtfListLevel.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel);
  FText := '';
  ListLevelProperties.Start := 1;
  ListLevelProperties.Separator := TdxCharacters.TabMark;
  ListLevelProperties.DisplayFormatString := '%0:s.';
  ListLevelProperties.RelativeRestartLevel := 0;
end;

function TdxRtfListLevel.CreateDisplayFormatString: string;
var
  APlaceholderIndices: TdxIntegerList;
begin
  if Length(Text) = 0 then
    Result := ''
  else
  begin
    APlaceholderIndices := CreatePlaceholderIndices;
    try
      Result := TdxStringHelper.RemoveSpecialSymbols(CreateDisplayFormatStringCore(APlaceholderIndices));
    finally
      APlaceholderIndices.Free;
    end;
  end;
end;

procedure TdxRtfListLevel.CopyFrom(AListLevel: TdxRtfListLevel);
begin
  inherited CopyFrom(AListLevel);
  FNumber := AListLevel.Number;
  FText := AListLevel.Text;
end;

function TdxRtfListLevel.Clone: TdxRtfListLevel;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxRtfListLevel.Create(DocumentModel);
  Result.CopyFrom(Self);
end;

function TdxRtfListLevel.CreatePlaceholderIndices: TdxIntegerList;
var
  I, ACount: Integer;
begin
  ACount := Min(9, Length(Number));
  Result := TdxIntegerList.Create;
  Result.Add(0);
  for I := 1 to ACount do
    if Ord(Number[I]) <= Length(Text) then
      Result.Add(Ord(Number[I]));
  Result.Add(Length(Text));
end;

function TdxRtfListLevel.CreateDisplayFormatStringCore(APlaceholderIndices: TdxIntegerList): string;
begin
  Result := TdxListLevelDisplayTextHelper.CreateDisplayFormatStringCore(APlaceholderIndices, Text);
end;

{ TdxRtfNumberingList }

constructor TdxRtfNumberingList.Create;
begin
  inherited Create;
  FLevels := TdxRtfListLevels.Create;
end;

destructor TdxRtfNumberingList.Destroy;
begin
  FreeAndNil(FLevels);
  inherited Destroy;
end;

procedure TdxRtfNumberingList.Reset;
begin
  FID := 0;
  FLevels.Clear;
  FName := '';
  FNumberingListType := TdxRtfNumberingListType.Unknown;
  FParentStyleID := 0;
  FStyleName := '';
end;

{ TdxRtfListOverrideLevel }

constructor TdxRtfListOverrideLevel.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
  FStartAt := MinInt;
end;

procedure TdxRtfListOverrideLevel.CopyFrom(ALevel: TdxRtfListOverrideLevel);
begin
  Level := ALevel.Level;
  OverrideFormat := ALevel.OverrideFormat;
  OverrideStartAt := ALevel.OverrideStartAt;
  StartAt := ALevel.StartAt;
end;

function TdxRtfListOverrideLevel.Clone: TdxRtfListOverrideLevel;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxRtfListOverrideLevel.Create(FDocumentModel);
  Result.CopyFrom(Self);
end;

procedure TdxRtfListOverrideLevel.SetLevel(const Value: TdxRtfListLevel);
begin
  FLevel := Value;
end;

{ TdxRtfNumberingListOverride }

constructor TdxRtfNumberingListOverride.Create;
begin
  inherited Create;
  FLevels := TdxRtfListOverrideLevels.Create;
end;

destructor TdxRtfNumberingListOverride.Destroy;
begin
  FreeAndNil(FLevels);
  inherited Destroy;
end;

{ TdxRtfSectionFormattingInfo }

constructor TdxRtfSectionFormattingInfo.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  FPage := TdxPageInfo.Create;
  FMargins := TdxMarginsInfo.Create;
  FPageNumbering := TdxPageNumberingInfo.Create;
  FGeneralSectionInfo := TdxGeneralSectionInfo.Create;
  FLineNumbering := TdxLineNumberingInfo.Create;
  FColumns := TdxColumnsInfo.Create;
  FFootNote := TdxSectionFootNote.Create(ADocumentModel);
  FEndNote := TdxSectionFootNote.Create(ADocumentModel);
end;

destructor TdxRtfSectionFormattingInfo.Destroy;
begin
  FreeAndNil(FEndNote);
  FreeAndNil(FFootNote);
  FreeAndNil(FColumns);
  FreeAndNil(FLineNumbering);
  FreeAndNil(FGeneralSectionInfo);
  FreeAndNil(FPageNumbering);
  FreeAndNil(FMargins);
  FreeAndNil(FPage);
  inherited Destroy;
end;

procedure TdxRtfSectionFormattingInfo.EnsureCurrentColumnExists;
var
  AColumnCollection: TdxList<TdxColumnInfo>;
  I: Integer;
begin
  AColumnCollection := Columns.Columns;
  if AColumnCollection.Count < FCurrentColumnIndex + 1 then
    for I := AColumnCollection.Count to FCurrentColumnIndex do
      AColumnCollection.Add(TdxColumnInfo.Create);
end;

procedure TdxRtfSectionFormattingInfo.Assign(Source: TPersistent);
var
  ASource: TdxRtfSectionFormattingInfo;
begin
  if Source is TdxRtfSectionFormattingInfo then
  begin
    ASource := TdxRtfSectionFormattingInfo(Source);
    FPage.CopyFrom(ASource.Page);
    FFootNote.CopyFrom(ASource.FootNote);
    FLineNumbering.CopyFrom(ASource.LineNumbering);
    FMargins.CopyFrom(ASource.Margins);
    FColumns.CopyFrom(ASource.Columns);
    FPageNumbering.CopyFrom(ASource.PageNumbering);
    FGeneralSectionInfo.CopyFrom(ASource.GeneralSectionInfo);
    FEndNote.CopyFrom(ASource.EndNote);
  end
  else
    inherited Assign(Source);
end;

procedure TdxRtfSectionFormattingInfo.InitializeDefault(ADocumentModel: TdxDocumentModel);
var
  ACache: TdxDocumentCache;
begin
  ACache := ADocumentModel.Cache;

  FMargins.CopyFrom(ACache.MarginsInfoCache[0]);
  FPageNumbering.CopyFrom(ACache.PageNumberingInfoCache[0]);
  FGeneralSectionInfo.CopyFrom(ACache.GeneralSectionInfoCache[0]);
  FLineNumbering.CopyFrom(ACache.LineNumberingInfoCache[0]);
  FColumns.CopyFrom(ACache.ColumnsInfoCache[0]);
  FFootNote.CopyFrom(ACache.FootNoteInfoCache[TdxFootNoteInfoCache.DefaultFootNoteItemIndex]);
  FEndNote.CopyFrom(ACache.FootNoteInfoCache[TdxFootNoteInfoCache.DefaultEndNoteItemIndex]);
end;

procedure TdxRtfSectionFormattingInfo.SetCurrentColumnSpace(Value: Integer);
begin
  EnsureCurrentColumnExists;
  FColumns.Columns[FCurrentColumnIndex].Space := Value;
end;

procedure TdxRtfSectionFormattingInfo.SetCurrentColumnWidth(Value: Integer);
begin
  EnsureCurrentColumnExists;
  FColumns.Columns[FCurrentColumnIndex].Width := Value;
end;

{ TdxRtfDocumentProperties }

constructor TdxRtfDocumentProperties.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  FColors := TdxRtfColorCollection.Create;
  FDefaultCodePage := TdxEncoding.GetEncodingCodePage(TdxRtfDocumentImporterOptions.DefaultEncoding);
  FFonts := TdxRtfFontInfoCollection.Create(ADocumentModel);
  FListTable := TdxRtfListTable.Create;
  FListOverrideTable := TdxListOverrideTable.Create;
  FDefaultSectionProperties := TdxRtfSectionFormattingInfo.Create(ADocumentModel);
  FDefaultSectionProperties.InitializeDefault(ADocumentModel);
  FUserNames := TdxStringList.Create;
end;

destructor TdxRtfDocumentProperties.Destroy;
begin
  FreeAndNil(FUserNames);
  FreeAndNil(FDefaultSectionProperties);
  FreeAndNil(FListOverrideTable);
  FreeAndNil(FListTable);
  FreeAndNil(FFonts);
  FreeAndNil(FColors);
  inherited Destroy;
end;

{ TdxRtfNumberingListInfo }

constructor TdxRtfNumberingListInfo.Create(ARtfNumberingListIndex, AListLevelIndex: Integer);
begin
  inherited Create;
  FRtfNumberingListIndex := ARtfNumberingListIndex;
  FListLevelIndex := AListLevelIndex;
end;

{ TdxRtfImporter }

constructor TdxRtfImporter.Create(ADocumentModel: TdxCustomDocumentModel;
  const AOptions: IdxImporterOptions);
begin
  inherited Create(ADocumentModel, AOptions);
  Assert(AOptions is TdxRtfDocumentImporterOptions);
  FOptionalGroupLevel := MaxInt;
  FDocumentProperties := TdxRtfDocumentProperties.Create(DocumentModel);
  FParagraphStyleCollectionIndex := TdxIntegersDictionary.Create;
  FParagraphStyleCollectionIndex.Add(0, 0);
  FParagraphStyleListOverrideIndexMap := TObjectDictionary<TdxParagraphStyle, TdxRtfNumberingListInfo>.Create([doOwnsValues]);
  FCharacterStyleCollectionIndex := TdxIntegersDictionary.Create;
  FLinkParagraphStyleIndexToCharacterStyleIndex := TdxIntegersDictionary.Create;
  FListOverrideIndexToNumberingListIndexMap := TDictionary<Integer, TdxNumberingListIndex>.Create;
  FNextParagraphStyleIndexTable := TdxIntegersDictionary.Create;
  FLastParagraphFormatting := nil;
  FLastParagraphPropertiesCacheIndex := -1;
  FKeyword := TStringBuilder.Create(32);
  FParameterValueString := TStringBuilder.Create(32);
  FTableStyleCollectionIndex := TdxIntegersDictionary.Create;
  FRtfLevels := TdxRtfListLevels.Create;
  FNumberingListToOldListLevelInfoMap := TObjectDictionary<TdxNumberingListIndex, TdxRtfOldListLevelInfo>.Create([doOwnsValues]);
  FLastParagraphFrameFormattingCacheIndex := -1;
  FLastParagraphFramePropertiesCacheIndex := -1;
  UpdateFields := True;
end;

destructor TdxRtfImporter.Destroy;
begin
  FreeAndNil(FLastParagraphFormatting);

  FreeAndNil(FNumberingListToOldListLevelInfoMap);
  FreeAndNil(FRtfLevels);
  FreeAndNil(FTableStyleCollectionIndex);
  FreeAndNil(FKeyword);
  FreeAndNil(FParameterValueString);
  FreeAndNil(FParagraphStyleListOverrideIndexMap);
  FreeAndNil(FListOverrideIndexToNumberingListIndexMap);
  FreeAndNil(FLinkParagraphStyleIndexToCharacterStyleIndex);
  FreeAndNil(FCharacterStyleCollectionIndex);
  FreeAndNil(FParagraphStyleCollectionIndex);
  FreeAndNil(FNextParagraphStyleIndexTable);
  FreeAndNil(FStateManager);
  FreeAndNil(FDocumentProperties);
  inherited Destroy;
end;

procedure TdxRtfImporter.ApplyCharacterProperties(
  ACharacterProperties: TdxCharacterProperties;
  ACharacterFormattingInfo: TdxCharacterFormattingInfo;
  AParentCharacterFormatting: TdxMergedCharacterProperties;
  AResetUse: Boolean = True);
var
  AParentCharacterInfo: TdxCharacterFormattingInfo;
begin
  ACharacterProperties.BeginUpdate;
  try
    if AResetUse then
      ACharacterProperties.ResetAllUse;
    AParentCharacterInfo := AParentCharacterFormatting.Info;
    if ACharacterFormattingInfo.AllCaps <> AParentCharacterInfo.AllCaps then
      ACharacterProperties.AllCaps := ACharacterFormattingInfo.AllCaps;
    if ACharacterFormattingInfo.Hidden <> AParentCharacterInfo.Hidden then
      ACharacterProperties.Hidden := ACharacterFormattingInfo.Hidden;
    if ACharacterFormattingInfo.FontBold <> AParentCharacterInfo.FontBold then
      ACharacterProperties.FontBold := ACharacterFormattingInfo.FontBold;
    if ACharacterFormattingInfo.FontItalic <> AParentCharacterInfo.FontItalic then
      ACharacterProperties.FontItalic := ACharacterFormattingInfo.FontItalic;
    if ACharacterFormattingInfo.FontName <> AParentCharacterInfo.FontName then
      ACharacterProperties.FontName := ACharacterFormattingInfo.FontName;
    if ACharacterFormattingInfo.DoubleFontSize <> AParentCharacterInfo.DoubleFontSize then
      ACharacterProperties.DoubleFontSize := ACharacterFormattingInfo.DoubleFontSize;
    if ACharacterFormattingInfo.FontUnderlineType <> AParentCharacterInfo.FontUnderlineType then
      ACharacterProperties.FontUnderlineType := ACharacterFormattingInfo.FontUnderlineType;
    if ACharacterFormattingInfo.ForeColor <> AParentCharacterInfo.ForeColor then
      ACharacterProperties.ForeColor := ACharacterFormattingInfo.ForeColor;
    if ACharacterFormattingInfo.BackColor <> AParentCharacterInfo.BackColor then
      ACharacterProperties.BackColor := ACharacterFormattingInfo.BackColor;
    if ACharacterFormattingInfo.Script <> AParentCharacterInfo.Script then
      ACharacterProperties.Script := ACharacterFormattingInfo.Script;
    if ACharacterFormattingInfo.StrikeoutColor <> AParentCharacterInfo.StrikeoutColor then
      ACharacterProperties.StrikeoutColor := ACharacterFormattingInfo.StrikeoutColor;
    if ACharacterFormattingInfo.FontStrikeoutType <> AParentCharacterInfo.FontStrikeoutType then
      ACharacterProperties.FontStrikeoutType := ACharacterFormattingInfo.FontStrikeoutType;
    if ACharacterFormattingInfo.StrikeoutWordsOnly <> AParentCharacterInfo.StrikeoutWordsOnly then
      ACharacterProperties.StrikeoutWordsOnly := ACharacterFormattingInfo.StrikeoutWordsOnly;
    if ACharacterFormattingInfo.UnderlineColor <> AParentCharacterInfo.UnderlineColor then
      ACharacterProperties.UnderlineColor := ACharacterFormattingInfo.UnderlineColor;
    if ACharacterFormattingInfo.UnderlineWordsOnly <> AParentCharacterInfo.UnderlineWordsOnly then
      ACharacterProperties.UnderlineWordsOnly := ACharacterFormattingInfo.UnderlineWordsOnly;
    if ACharacterFormattingInfo.NoProof <> AParentCharacterInfo.NoProof then
      ACharacterProperties.NoProof := ACharacterFormattingInfo.NoProof;
  finally
    ACharacterProperties.EndUpdate;
  end;
end;

procedure TdxRtfImporter.ApplyFormattingToLastParagraph;
var
  AParagraph: TdxParagraph;
begin
  Position.ParagraphFormattingInfo.LineSpacing := Position.ParagraphFormattingInfo.CalcLineSpacing(UnitConverter);
  Position.ParagraphFormattingInfo.LineSpacingType := Position.ParagraphFormattingInfo.CalcLineSpacingType;
  AParagraph := PieceTable.Paragraphs.Last;
  ApplyParagraphFormatting(AParagraph.Index, False);
  PieceTable.InheritParagraphRunStyleCore(Position, TdxParagraphRun(PieceTable.Runs[AParagraph.LastRunIndex]));
end;

procedure TdxRtfImporter.ApplyLineSpacing(AParagraphFormatting: TdxRtfParagraphFormattingInfo);
begin
  AParagraphFormatting.LineSpacingType := AParagraphFormatting.CalcLineSpacingType;
  AParagraphFormatting.LineSpacing := AParagraphFormatting.CalcLineSpacing(UnitConverter);
end;

procedure TdxRtfImporter.ApplyParagraphFrameFormatting(AFrameProperties: TdxFrameProperties; AParagraphFrameFormatting: TdxParagraphFrameFormattingInfo);
var
  AParagraphFrameFormattingCacheIndex: Integer;
  AProperties: TdxParagraphFrameFormattingBase;
begin
  Assert(not AFrameProperties.IsUpdateLocked);
  AParagraphFrameFormattingCacheIndex := DocumentModel.Cache.ParagraphFrameFormattingInfoCache.GetItemIndex(AParagraphFrameFormatting);
  if AParagraphFrameFormattingCacheIndex = FLastParagraphFrameFormattingCacheIndex then
    AFrameProperties.SetIndexInitial(FLastParagraphFramePropertiesCacheIndex)
  else
  begin
    AProperties := TdxParagraphFrameFormattingBase.Create(PieceTable, DocumentModel,
      AParagraphFrameFormatting, TdxParagraphFrameFormattingOptions.RootParagraphFrameFormattingOption);
    try
      AFrameProperties.ReplaceInfo(AProperties,
        AFrameProperties.BatchUpdateChangeActions);
    finally
      AProperties.Free;
    end;
  end;
  FLastParagraphFrameFormattingCacheIndex := AParagraphFrameFormattingCacheIndex;
  FLastParagraphFramePropertiesCacheIndex := AFrameProperties.Index;
end;

procedure TdxRtfImporter.ApplyFrameProperties(AParagraphIndex: TdxParagraphIndex);
var
  AParagraph: TdxParagraph;
  AParagraphFrameFormatting: TdxParagraphFrameFormattingInfo;
  AMergedFrameProperties: TdxMergedFrameProperties;
begin
  if not DocumentModel.DocumentCapabilities.ParagraphFramesAllowed then
    Exit;
  AParagraph := PieceTable.Paragraphs[AParagraphIndex];
  AParagraphFrameFormatting := Position.ParagraphFrameFormattingInfo;
  AMergedFrameProperties := AParagraph.ParagraphStyle.GetMergedFrameProperties;
  try
    if AParagraphFrameFormatting <> nil then
    begin
      if not IsContainsParagraphFrame then
        IsContainsParagraphFrame := True;
      if (AMergedFrameProperties = nil) or not AParagraphFrameFormatting.Equals(AMergedFrameProperties.Info) then
        ApplyParagraphFrameFormatting(AParagraph, AParagraphFrameFormatting);
    end
    else
    begin
      if AMergedFrameProperties <> nil then
        AParagraph.SetDefaultFrameProperties;
    end;
  finally
    AMergedFrameProperties.Free;
  end;
end;

procedure TdxRtfImporter.ApplyParagraphProperties(AParagraphProperties: TdxParagraphProperties;
  AParagraphFormatting: TdxParagraphFormattingInfo; AParentParagraphProperties: TdxMergedParagraphProperties;
  AResetUse: Boolean = True);
var
  AParentParagraphInfo: TdxParagraphFormattingInfo;
  ActualLeftIndent: Integer;
  AShouldApplyLineSpacing: Boolean;
begin
  AParagraphProperties.BeginUpdate;
  try
    AParentParagraphInfo := AParentParagraphProperties.Info;
    if AResetUse then
      AParagraphProperties.ResetAllUse;
    if AParagraphFormatting.Alignment <> AParentParagraphInfo.Alignment then
      AParagraphProperties.Alignment := AParagraphFormatting.Alignment;
    if AParagraphFormatting.RightIndent <> AParentParagraphInfo.RightIndent then
      AParagraphProperties.RightIndent := AParagraphFormatting.RightIndent;
    if AParagraphFormatting.FirstLineIndentType <> AParentParagraphInfo.FirstLineIndentType then
      AParagraphProperties.FirstLineIndentType := AParagraphFormatting.FirstLineIndentType;
    ActualLeftIndent := AParagraphFormatting.LeftIndent;
    if ActualLeftIndent <> AParentParagraphInfo.LeftIndent then
      AParagraphProperties.LeftIndent := ActualLeftIndent;
    if AParagraphFormatting.FirstLineIndent <> AParentParagraphInfo.FirstLineIndent then
      AParagraphProperties.FirstLineIndent := AParagraphFormatting.FirstLineIndent;
    if AParagraphFormatting.SuppressHyphenation <> AParentParagraphInfo.SuppressHyphenation then
      AParagraphProperties.SuppressHyphenation := AParagraphFormatting.SuppressHyphenation;
    if AParagraphFormatting.SuppressLineNumbers <> AParentParagraphInfo.SuppressLineNumbers then
      AParagraphProperties.SuppressLineNumbers := AParagraphFormatting.SuppressLineNumbers;
    if AParagraphFormatting.ContextualSpacing <> AParentParagraphInfo.ContextualSpacing then
      AParagraphProperties.ContextualSpacing := AParagraphFormatting.ContextualSpacing;
    if AParagraphFormatting.PageBreakBefore <> AParentParagraphInfo.PageBreakBefore then
      AParagraphProperties.PageBreakBefore := AParagraphFormatting.PageBreakBefore;
    if AParagraphFormatting.BeforeAutoSpacing <> AParentParagraphInfo.BeforeAutoSpacing then
      AParagraphProperties.BeforeAutoSpacing := AParagraphFormatting.BeforeAutoSpacing;
    if AParagraphFormatting.AfterAutoSpacing <> AParentParagraphInfo.AfterAutoSpacing then
      AParagraphProperties.AfterAutoSpacing := AParagraphFormatting.AfterAutoSpacing;
    if AParagraphFormatting.KeepWithNext <> AParentParagraphInfo.KeepWithNext then
      AParagraphProperties.KeepWithNext := AParagraphFormatting.KeepWithNext;
    if AParagraphFormatting.KeepLinesTogether <> AParentParagraphInfo.KeepLinesTogether then
      AParagraphProperties.KeepLinesTogether := AParagraphFormatting.KeepLinesTogether;
    if AParagraphFormatting.WidowOrphanControl <> AParentParagraphInfo.WidowOrphanControl then
      AParagraphProperties.WidowOrphanControl := aparagraphFormatting.WidowOrphanControl;
    if AParagraphFormatting.OutlineLevel <> AParentParagraphInfo.OutlineLevel then
      AParagraphProperties.OutlineLevel := AParagraphFormatting.OutlineLevel;
    if AParagraphFormatting.BackColor <> AParentParagraphInfo.BackColor then
      AParagraphProperties.BackColor := AParagraphFormatting.BackColor;
    if AParagraphFormatting.SpacingBefore <> AParentParagraphInfo.SpacingBefore then
      AParagraphProperties.SpacingBefore := AParagraphFormatting.SpacingBefore;
    if AParagraphFormatting.SpacingAfter <> AParentParagraphInfo.SpacingAfter then
      AParagraphProperties.SpacingAfter := AParagraphFormatting.SpacingAfter;
    AShouldApplyLineSpacing := (AParagraphFormatting.LineSpacingType <> AParentParagraphInfo.LineSpacingType) or
      (AParagraphFormatting.LineSpacing <> AParentParagraphInfo.LineSpacing);
    if AShouldApplyLineSpacing then
    begin
      AParagraphProperties.LineSpacingType := AParagraphFormatting.LineSpacingType;
      AParagraphProperties.LineSpacing := AParagraphFormatting.LineSpacing;
    end;
    if not AParagraphFormatting.LeftBorder.Equals(AParentParagraphInfo.LeftBorder) then
      AParagraphProperties.LeftBorder := AParagraphFormatting.LeftBorder;
    if not AParagraphFormatting.RightBorder.Equals(AParentParagraphInfo.RightBorder) then
      AParagraphProperties.RightBorder := AParagraphFormatting.RightBorder;
    if not AParagraphFormatting.TopBorder.Equals(AParentParagraphInfo.TopBorder) then
      AParagraphProperties.TopBorder := AParagraphFormatting.TopBorder;
    if not AParagraphFormatting.BottomBorder.Equals(AParentParagraphInfo.BottomBorder) then
      AParagraphProperties.BottomBorder := AParagraphFormatting.BottomBorder;
  finally
    AParagraphProperties.EndUpdate;
  end;
end;

procedure TdxRtfImporter.ApplyParagraphProperties(
  AParagraphProperties: TdxParagraphProperties;
  AParentParagraphRtfStyleIndex: Integer;
  AParagraphFormatting: TdxRtfParagraphFormattingInfo);
var
  AParentParagraphProperties: TdxMergedParagraphProperties;
begin
  AParentParagraphProperties := GetStyleMergedParagraphProperties(AParentParagraphRtfStyleIndex);
  try
    ApplyLineSpacing(AParagraphFormatting);
    ApplyParagraphProperties(AParagraphProperties, AParagraphFormatting, AParentParagraphProperties);
  finally
    AParentParagraphProperties.Free;
  end;
end;

procedure TdxRtfImporter.ApplyTableCellProperties(
  ACellProperties: TdxTableCellProperties;
  AParentCellProperties: TdxMergedTableCellProperties);
var
  AParentCellPropertiesInfo: TdxCombinedCellPropertiesInfo;
  ACellPropertiesInfo: TdxCombinedCellPropertiesInfo;
begin
  ACellProperties.ResetAllUse;
  AParentCellPropertiesInfo := AParentCellProperties.Info;
  ACellPropertiesInfo := TdxCombinedCellPropertiesInfo.Create(ACellProperties);
  try
    if not ACellPropertiesInfo.Borders.TopBorder.Equals(AParentCellPropertiesInfo.Borders.TopBorder) then
      ACellProperties.Borders.TopBorder.CopyFrom(ACellPropertiesInfo.Borders.TopBorder);
    if not ACellPropertiesInfo.Borders.RightBorder.Equals(AParentCellPropertiesInfo.Borders.RightBorder) then
      ACellProperties.Borders.RightBorder.CopyFrom(ACellPropertiesInfo.Borders.RightBorder);
    if not ACellPropertiesInfo.Borders.LeftBorder.Equals(AParentCellPropertiesInfo.Borders.LeftBorder) then
      ACellProperties.Borders.LeftBorder.CopyFrom(ACellPropertiesInfo.Borders.LeftBorder);
    if not ACellPropertiesInfo.Borders.BottomBorder.Equals(AParentCellPropertiesInfo.Borders.BottomBorder) then
      ACellProperties.Borders.BottomBorder.CopyFrom(ACellPropertiesInfo.Borders.BottomBorder);
    if not ACellPropertiesInfo.Borders.InsideHorizontalBorder.Equals(AParentCellPropertiesInfo.Borders.InsideHorizontalBorder) then
      ACellProperties.Borders.InsideHorizontalBorder.CopyFrom(ACellPropertiesInfo.Borders.InsideHorizontalBorder);
    if not ACellPropertiesInfo.Borders.InsideVerticalBorder.Equals(AParentCellPropertiesInfo.Borders.InsideVerticalBorder) then
      ACellProperties.Borders.InsideVerticalBorder.CopyFrom(ACellPropertiesInfo.Borders.InsideVerticalBorder);
    if not ACellPropertiesInfo.Borders.TopLeftDiagonalBorder.Equals(AParentCellPropertiesInfo.Borders.TopLeftDiagonalBorder) then
      ACellProperties.Borders.TopLeftDiagonalBorder.CopyFrom(ACellPropertiesInfo.Borders.TopLeftDiagonalBorder);
    if not ACellPropertiesInfo.Borders.TopRightDiagonalBorder.Equals(AParentCellPropertiesInfo.Borders.TopRightDiagonalBorder) then
      ACellProperties.Borders.TopRightDiagonalBorder.CopyFrom(ACellPropertiesInfo.Borders.TopRightDiagonalBorder);

    if not ACellPropertiesInfo.CellMargins.Top.Equals(AParentCellPropertiesInfo.CellMargins.Top) then
      ACellProperties.CellMargins.Top.CopyFrom(ACellPropertiesInfo.CellMargins.Top);
    if not ACellPropertiesInfo.CellMargins.Right.Equals(AParentCellPropertiesInfo.CellMargins.Right) then
      ACellProperties.CellMargins.Right.CopyFrom(ACellPropertiesInfo.CellMargins.Right);
    if not ACellPropertiesInfo.CellMargins.Left.Equals(AParentCellPropertiesInfo.CellMargins.Left) then
      ACellProperties.CellMargins.Left.CopyFrom(ACellPropertiesInfo.CellMargins.Left);
    if not ACellPropertiesInfo.CellMargins.Bottom.Equals(AParentCellPropertiesInfo.CellMargins.Bottom) then
      ACellProperties.CellMargins.Bottom.CopyFrom(ACellPropertiesInfo.CellMargins.Bottom);

    if ACellPropertiesInfo.GeneralSettings.BackgroundColor <> AParentCellPropertiesInfo.GeneralSettings.BackgroundColor then
      ACellProperties.BackgroundColor := ACellPropertiesInfo.GeneralSettings.BackgroundColor;
    if ACellPropertiesInfo.GeneralSettings.ForegroundColor <> AParentCellPropertiesInfo.GeneralSettings.ForegroundColor then
      ACellProperties.ForegroundColor := ACellPropertiesInfo.GeneralSettings.ForegroundColor;
    if ACellPropertiesInfo.GeneralSettings.Shading <> AParentCellPropertiesInfo.GeneralSettings.Shading then
      ACellProperties.Shading := ACellPropertiesInfo.GeneralSettings.Shading;
    if ACellPropertiesInfo.GeneralSettings.CellConditionalFormatting <> AParentCellPropertiesInfo.GeneralSettings.CellConditionalFormatting then
      ACellProperties.CellConditionalFormatting := ACellPropertiesInfo.GeneralSettings.CellConditionalFormatting;
    if ACellPropertiesInfo.GeneralSettings.ColumnSpan <> AParentCellPropertiesInfo.GeneralSettings.ColumnSpan then
      ACellProperties.ColumnSpan := ACellPropertiesInfo.GeneralSettings.ColumnSpan;
    if ACellPropertiesInfo.GeneralSettings.FitText <> AParentCellPropertiesInfo.GeneralSettings.FitText then
      ACellProperties.FitText := ACellPropertiesInfo.GeneralSettings.FitText;
    if ACellPropertiesInfo.GeneralSettings.HideCellMark <> AParentCellPropertiesInfo.GeneralSettings.HideCellMark then
      ACellProperties.HideCellMark := ACellPropertiesInfo.GeneralSettings.HideCellMark;
    if ACellPropertiesInfo.GeneralSettings.NoWrap <> AParentCellPropertiesInfo.GeneralSettings.NoWrap then
      ACellProperties.NoWrap := ACellPropertiesInfo.GeneralSettings.NoWrap;
    if ACellPropertiesInfo.GeneralSettings.TextDirection <> AParentCellPropertiesInfo.GeneralSettings.TextDirection then
      ACellProperties.TextDirection := ACellPropertiesInfo.GeneralSettings.TextDirection;
    if ACellPropertiesInfo.GeneralSettings.VerticalAlignment <> AParentCellPropertiesInfo.GeneralSettings.VerticalAlignment then
      ACellProperties.VerticalAlignment := ACellPropertiesInfo.GeneralSettings.VerticalAlignment;
    if ACellPropertiesInfo.GeneralSettings.VerticalMerging <> AParentCellPropertiesInfo.GeneralSettings.VerticalMerging then
      ACellProperties.VerticalMerging := ACellPropertiesInfo.GeneralSettings.VerticalMerging;

    if not ACellPropertiesInfo.PreferredWidth.Equals(AParentCellPropertiesInfo.PreferredWidth) then
      ACellProperties.PreferredWidth.CopyFrom(ACellPropertiesInfo.PreferredWidth);
  finally
    ACellPropertiesInfo.Free;
  end;
end;

procedure TdxRtfImporter.ApplyTabs(ATarget: TdxTabProperties; AParentParagraphRtfStyleIndex: Integer; AImportedTabs: TdxTabFormattingInfo);
var
  AParentTabs, ANormalizedTabs: TdxTabFormattingInfo;
begin
  AParentTabs := GetStyleMergedTabs(AParentParagraphRtfStyleIndex);
  try
    ANormalizedTabs := TdxTabFormattingInfo.NormalizeTabs(AImportedTabs, AParentTabs);
    try
      if ANormalizedTabs <> nil then
        ATarget.SetTabs(ANormalizedTabs)
      else
        ATarget.SetTabs(AImportedTabs);
    finally
      ANormalizedTabs.Free;
    end;
  finally
    AParentTabs.Free;
  end;
end;

function TdxRtfImporter.GetStyleMergedTabs(ARtfStyleIndex: Integer): TdxTabFormattingInfo;
var
  AStyleIndex: Integer;
begin
  AStyleIndex := GetParagraphStyleIndex(ARtfStyleIndex);
  Result := DocumentModel.ParagraphStyles[AStyleIndex].GetTabs;
end;

procedure TdxRtfImporter.ApplyTableProperties(ATableProperties: TdxTableProperties;
  AParentTableProperties: TdxMergedTableProperties);
var
  AParentTablePropertiesInfo: TdxCombinedTablePropertiesInfo;
  ATablePropertiesInfo: TdxCombinedTablePropertiesInfo;
begin
  ATableProperties.ResetAllUse;
  AParentTablePropertiesInfo := AParentTableProperties.Info;
  ATablePropertiesInfo := TdxCombinedTablePropertiesInfo.Create(ATableProperties);
  try
    if not ATablePropertiesInfo.Borders.TopBorder.Equals(AParentTablePropertiesInfo.Borders.TopBorder) then
      ATableProperties.Borders.TopBorder.CopyFrom(ATablePropertiesInfo.Borders.TopBorder);
    if not ATablePropertiesInfo.Borders.RightBorder.Equals(AParentTablePropertiesInfo.Borders.RightBorder) then
      ATableProperties.Borders.RightBorder.CopyFrom(ATablePropertiesInfo.Borders.RightBorder);
    if not ATablePropertiesInfo.Borders.LeftBorder.Equals(AParentTablePropertiesInfo.Borders.LeftBorder) then
      ATableProperties.Borders.LeftBorder.CopyFrom(ATablePropertiesInfo.Borders.LeftBorder);
    if not ATablePropertiesInfo.Borders.BottomBorder.Equals(AParentTablePropertiesInfo.Borders.BottomBorder) then
      ATableProperties.Borders.BottomBorder.CopyFrom(ATablePropertiesInfo.Borders.BottomBorder);
    if not ATablePropertiesInfo.Borders.InsideHorizontalBorder.Equals(AParentTablePropertiesInfo.Borders.InsideHorizontalBorder) then
      ATableProperties.Borders.InsideHorizontalBorder.CopyFrom(ATablePropertiesInfo.Borders.InsideHorizontalBorder);
    if not ATablePropertiesInfo.Borders.InsideVerticalBorder.Equals(AParentTablePropertiesInfo.Borders.InsideVerticalBorder) then
      ATableProperties.Borders.InsideVerticalBorder.CopyFrom(ATablePropertiesInfo.Borders.InsideVerticalBorder);
    if not ATablePropertiesInfo.CellMargins.Top.Equals(AParentTablePropertiesInfo.CellMargins.Top) then
      ATableProperties.CellMargins.Top.CopyFrom(ATablePropertiesInfo.CellMargins.Top);
    if not ATablePropertiesInfo.CellMargins.Right.Equals(AParentTablePropertiesInfo.CellMargins.Right) then
      ATableProperties.CellMargins.Right.CopyFrom(ATablePropertiesInfo.CellMargins.Right);
    if not ATablePropertiesInfo.CellMargins.Left.Equals(AParentTablePropertiesInfo.CellMargins.Left) then
      ATableProperties.CellMargins.Left.CopyFrom(ATablePropertiesInfo.CellMargins.Left);
    if not ATablePropertiesInfo.CellMargins.Bottom.Equals(AParentTablePropertiesInfo.CellMargins.Bottom) then
      ATableProperties.CellMargins.Bottom.CopyFrom(ATablePropertiesInfo.CellMargins.Bottom);
    if not ATablePropertiesInfo.CellSpacing.Equals(AParentTablePropertiesInfo.CellSpacing) then
      ATableProperties.CellSpacing.CopyFrom(ATablePropertiesInfo.CellSpacing);
    if not ATablePropertiesInfo.FloatingPosition.Equals(AParentTablePropertiesInfo.FloatingPosition) then
      ATableProperties.FloatingPosition.CopyFrom(ATablePropertiesInfo.FloatingPosition);
    if ATablePropertiesInfo.GeneralSettings.IsTableOverlap <> AParentTablePropertiesInfo.GeneralSettings.IsTableOverlap then
      ATableProperties.IsTableOverlap := ATablePropertiesInfo.GeneralSettings.IsTableOverlap;
    if ATablePropertiesInfo.GeneralSettings.TableLayout <> AParentTablePropertiesInfo.GeneralSettings.TableLayout then
      ATableProperties.TableLayout := ATablePropertiesInfo.GeneralSettings.TableLayout;
    if ATablePropertiesInfo.GeneralSettings.TableLook <> AParentTablePropertiesInfo.GeneralSettings.TableLook then
      ATableProperties.TableLook := ATablePropertiesInfo.GeneralSettings.TableLook;
    if ATablePropertiesInfo.GeneralSettings.TableStyleColBandSize <> AParentTablePropertiesInfo.GeneralSettings.TableStyleColBandSize then
      ATableProperties.TableStyleColBandSize := ATablePropertiesInfo.GeneralSettings.TableStyleColBandSize;
    if ATablePropertiesInfo.GeneralSettings.TableStyleRowBandSize <> AParentTablePropertiesInfo.GeneralSettings.TableStyleRowBandSize then
      ATableProperties.TableStyleRowBandSize := ATablePropertiesInfo.GeneralSettings.TableStyleRowBandSize;
    if not ATablePropertiesInfo.PreferredWidth.Equals(AParentTablePropertiesInfo.PreferredWidth) then
      ATableProperties.PreferredWidth.CopyFrom(ATablePropertiesInfo.PreferredWidth);
    if not ATablePropertiesInfo.TableIndent.Equals(AParentTablePropertiesInfo.TableIndent) then
      ATableProperties.TableIndent.CopyFrom(ATablePropertiesInfo.TableIndent);
  finally
    ATablePropertiesInfo.Free;
  end;
end;

procedure TdxRtfImporter.ApplyTableRowProperties(
  ARowProperties: TdxTableRowProperties;
  AParentRowProperties: TdxMergedTableRowProperties);
var
  AParentRowPropertiesInfo: TdxCombinedTableRowPropertiesInfo;
  ARowPropertiesInfo: TdxCombinedTableRowPropertiesInfo;
begin
  ARowProperties.ResetAllUse;
  AParentRowPropertiesInfo := AParentRowProperties.Info;
  ARowPropertiesInfo := TdxCombinedTableRowPropertiesInfo.Create(ARowProperties);
  try
    if not ARowPropertiesInfo.CellSpacing.Equals(AParentRowPropertiesInfo.CellSpacing) then
      ARowProperties.CellSpacing.CopyFrom(ARowPropertiesInfo.CellSpacing);
    if ARowPropertiesInfo.GeneralSettings.CantSplit <> AParentRowPropertiesInfo.GeneralSettings.CantSplit then
      ARowProperties.CantSplit := ARowPropertiesInfo.GeneralSettings.CantSplit;
    if ARowPropertiesInfo.GeneralSettings.GridAfter <> AParentRowPropertiesInfo.GeneralSettings.GridAfter then
      ARowProperties.GridAfter := ARowPropertiesInfo.GeneralSettings.GridAfter;
    if ARowPropertiesInfo.GeneralSettings.GridBefore <> AParentRowPropertiesInfo.GeneralSettings.GridBefore then
      ARowProperties.GridBefore := ARowPropertiesInfo.GeneralSettings.GridBefore;
    if ARowPropertiesInfo.GeneralSettings.Header <> AParentRowPropertiesInfo.GeneralSettings.Header then
      ARowProperties.Header := ARowPropertiesInfo.GeneralSettings.Header;
    if ARowPropertiesInfo.GeneralSettings.HideCellMark <> AParentRowPropertiesInfo.GeneralSettings.HideCellMark then
      ARowProperties.HideCellMark := ARowPropertiesInfo.GeneralSettings.HideCellMark;
    if ARowPropertiesInfo.GeneralSettings.TableRowAlignment <> AParentRowPropertiesInfo.GeneralSettings.TableRowAlignment then
      ARowProperties.TableRowAlignment := ARowPropertiesInfo.GeneralSettings.TableRowAlignment;

    if not ARowPropertiesInfo.Height.Equals(AParentRowPropertiesInfo.Height) then
      ARowProperties.Height.CopyFrom(ARowPropertiesInfo.Height);
    if not ARowPropertiesInfo.WidthAfter.Equals(AParentRowPropertiesInfo.WidthAfter) then
      ARowProperties.WidthAfter.CopyFrom(ARowPropertiesInfo.WidthAfter);
    if not ARowPropertiesInfo.WidthBefore.Equals(AParentRowPropertiesInfo.WidthBefore) then
      ARowProperties.WidthBefore.CopyFrom(ARowPropertiesInfo.WidthBefore);
  finally
    ARowPropertiesInfo.Free;
  end;
end;

procedure TdxRtfImporter.ApplyParagraphFormatting(AParagraphIndex: TdxParagraphIndex; ASectionBreak: Boolean);
var
  AParagraph: TdxParagraph;
  AParagraphFormatting: TdxRtfParagraphFormattingInfo;
  AListIndex: TdxNumberingListIndex;
  AListLevelIndex: Integer;
begin
  AParagraph := PieceTable.Paragraphs[AParagraphIndex];
  AParagraphFormatting := Position.ParagraphFormattingInfo;
  ApplyParagraphFormattingCore(AParagraph, AParagraphFormatting);
  if TdxDocumentFormatsHelper.ShouldInsertNumbering(DocumentModel) then
  begin
    AListIndex := AParagraphFormatting.NumberingListIndex;
    if (AListIndex >= 0) or (AListIndex = NumberingListIndexNoNumberingList) then
    begin
      if not (ASectionBreak and AParagraph.IsEmpty) then
      begin
        AListLevelIndex := AParagraphFormatting.ListLevelIndex;
        if (AListLevelIndex < 0) or (AListLevelIndex > 12) or (AListLevelIndex = 9) then
          AListLevelIndex := 0;
        if AListIndex >= NumberingListIndexMinValue then
          AddNumberingListToParagraph(AParagraph, AListIndex, AListLevelIndex)
        else
          PieceTable.AddNumberingListToParagraph(AParagraph, AListIndex, 0);
      end;
    end
    else
      if not Position.CurrentOldListSkipNumbering then
      begin
        Position.CurrentOldMultiLevelListIndex := NumberingListIndexNoNumberingList;
        Position.CurrentOldSimpleListIndex := NumberingListIndexNoNumberingList
      end;
  end;
  AParagraph.SetOwnTabs(AParagraphFormatting.Tabs);
  if Position.ParagraphFormattingInfo.RtfTableStyleIndexForRowOrCell <> 0 then
    PieceTableInfo.ParagraphTableStyles.AddOrSetValue(AParagraphIndex, Position.ParagraphFormattingInfo.RtfTableStyleIndexForRowOrCell);
end;

procedure TdxRtfImporter.ApplySectionFormatting(ASkipNumbering: Boolean = False);
var
  ASectionFormatting: TdxRtfSectionFormattingInfo;
  ASections: TdxSectionCollection;
  ASection: TdxSection;
  AParagraphFormatting: TdxRtfParagraphFormattingInfo;
  AListIndex: TdxNumberingListIndex;
  AParagraph: TdxParagraph;
begin
  ASectionFormatting := Position.SectionFormattingInfo;
  ASections := DocumentModel.Sections;
  ASection := ASections.Last;
  ASectionFormatting.Page.ValidatePaperKind(DocumentModel.UnitConverter);
  ASection.Page.CopyFrom(ASectionFormatting.Page);
  ASection.Margins.CopyFrom(ASectionFormatting.Margins);
  ASection.PageNumbering.CopyFrom(ASectionFormatting.PageNumbering);
  ASection.GeneralSettings.CopyFrom(ASectionFormatting.GeneralSectionInfo);
  ASection.LineNumbering.CopyFrom(ASectionFormatting.LineNumbering);
  ASection.Columns.CopyFrom(ASectionFormatting.Columns);
  ASection.FootNote.CopyFrom(ASectionFormatting.FootNote);
  ASection.EndNote.CopyFrom(ASectionFormatting.EndNote);

  AParagraphFormatting := Position.ParagraphFormattingInfo;
  AListIndex := AParagraphFormatting.NumberingListIndex;
  if not ASkipNumbering and (AListIndex >= 0) and TdxDocumentFormatsHelper.ShouldInsertNumbering(DocumentModel) then
  begin
    AParagraph := PieceTable.Paragraphs[Position.ParagraphIndex];
    if not AParagraph.IsInList then
      AddNumberingListToParagraph(AParagraph, AParagraphFormatting.NumberingListIndex, AParagraphFormatting.ListLevelIndex);
  end
  else
  begin
    Position.CurrentOldSimpleListIndex := NumberingListIndexNoNumberingList;
    Position.CurrentOldMultiLevelListIndex := NumberingListIndexNoNumberingList;
  end;
  if ((ASections.Count = 1) or ASectionFormatting.RestartPageNumbering) and
    (ASectionFormatting.PageNumbering.FirstPageNumber >= 0) then
  begin
    ASection.PageNumbering.FirstPageNumber := ASectionFormatting.PageNumbering.FirstPageNumber;
    ASection.PageNumbering.ContinueNumbering := False;
  end
  else
  begin
    ASection.PageNumbering.FirstPageNumber := -1;
    ASection.PageNumbering.ContinueNumbering := True;
  end;
end;

function TdxRtfImporter.ReadByte(AStream: TStream; out AByte: Byte): Boolean;
begin
  Result := AStream.Read(AByte, 1) > 0;
  if not Result then
    AByte := 0;
end;

function TdxRtfImporter.ReadChar(AStream: TStream; out AChar: Char): Boolean;
var
  AOrd: Byte;
begin
  Result := ReadByte(AStream, AOrd);
  AChar := Char(AOrd)
end;

procedure TdxRtfImporter.ParseCharWithoutDecoding(AChar: Char);
begin
  FlushDecoder;
  if FSkipCount = 0 then
    Destination.ProcessChar(AChar);
  DecreaseSkipCount;
end;

procedure TdxRtfImporter.InitializeStateManager;
begin
  Assert(stateManager = nil, 'stateManager already created');
  FStateManager := TdxRichEditRtfParserStateManager.Create(Self);
  FStateManager.Initialize;
end;

procedure TdxRtfImporter.ImportCore(AStream: TStream);
var
  AUpdateOptions: TdxFieldUpdateOnLoadOptions;
begin
  DocumentModel.BeginSetContent;
  try
    InitializeStateManager;
    FStream := AStream;
    try
      ImportRtfStream(AStream);
    finally
      FStream := nil;
    end;
  finally
    AUpdateOptions := Options.UpdateField.GetNativeOptions;
    try
      DocumentModel.EndSetContent(TdxDocumentModelChangeType.LoadNewDocument, UpdateFields, Options.PasteFromIE, AUpdateOptions);
    finally
      AUpdateOptions.Free;
    end;
  end;
end;

procedure TdxRtfImporter.ImportRtfStream(AStream: TStream);
var
  AByte: Byte;
  AChar, ALastReadChar: Char;
  AOrdChar, ANextChar, AProgressCount: Integer;
  AParsingState: TdxRichEditRtfParsingState;
  ALastDestination: TdxDefaultDestination;
begin
{$IFDEF AQTIME}
  EnableProfiling(True);
{$ENDIF}
  DocumentModel.DefaultCharacterProperties.FontName := 'Times New Roman';
  DocumentModel.DefaultCharacterProperties.DoubleFontSize := 24;
  ClearNumberingList;
  if not CheckSignature(AStream) then
    Exit;
  ReadChar(AStream, AChar);
  Assert(AChar = '{');
  ANextChar := -1;
  AProgressCount := 0;
  while True do
  begin
    if ANextChar = -1 then
    begin
      if not ReadByte(AStream, AByte) then
        AOrdChar := -1
      else
        AOrdChar := AByte;
    end
    else
    begin
      AOrdChar := ANextChar;
      ANextChar := -1;
    end;
    if AOrdChar < 0 then
      Break;
    Inc(AProgressCount);
    AChar := Char(AOrdChar);
    AParsingState := StateManager.ParsingState;
    if AParsingState = TdxRichEditRtfParsingState.BinData then
      ParseBinChar(AChar)
    else
    begin
      case AChar of
        '{':
          begin
            FlushDecoder;
            PushRtfState;
          end;
        '}':
          begin
            FlushDecoder;
            if PopRtfState = TdxRichEditPopRtfState.StackEmpty then
              SkipDataBeyondOuterBrace(AStream);
          end;
        '\':
          begin
            ALastReadChar := ParseRtfKeyword(AStream);
            if ALastReadChar <> ' ' then
              ANextChar := Ord(ALastReadChar);
          end;
        #13:
          ParseCR();
        #10:
          ParseLF();
      else
        if AParsingState = TdxRichEditRtfParsingState.Normal then
          ParseChar(AChar)
        else
          ParseHexChar(AStream, AChar);
      end;
      if AProgressCount mod 1024 = 0 then
        ProgressIndication.SetProgress(FStream.Position);
    end;
  end;
  while StateManager.States.Count > 0 do
    PopRtfState;

  if BinCharCount <> 0 then
    ThrowUnexpectedEndOfFile;
  if FStateManager.States.Count > 0 then
    ThrowUnexpectedEndOfFile;
  if Fields.Count <> 0 then
    ThrowInvalidRtfFile;
  Assert(Destination <> nil);
  ALastDestination := Safe<TdxDefaultDestination>.Cast(Destination);
  if ALastDestination = nil then
    TdxRichEditExceptions.ThrowInternalException;
{$IFDEF AQTIME}
  EnableProfiling(False);
{$ENDIF}
  ALastDestination.FinalizePieceTableCreation;
  CheckTablesStructure;
{$IFDEF AQTIME}
  EnableProfiling(False);
{$ENDIF}
end;

class function TdxRtfImporter.IsAlpha(C: Char): Boolean;
begin
  Result := ((C >= 'a') and (C <= 'z')) or ((C >= 'A') and (C <= 'Z'));
end;

class function TdxRtfImporter.IsDigit(C: Char): Boolean;
begin
  Result := (C >= '0') and (C <= '9');
end;

procedure TdxRtfImporter.ParseCR;
begin
  Assert(StateManager.ParsingState = TdxRichEditRtfParsingState.Normal);
end;

procedure TdxRtfImporter.ParseLF;
begin
  Assert(StateManager.ParsingState = TdxRichEditRtfParsingState.Normal);
end;

function TdxRtfImporter.ParseRtfKeyword(AStream: TStream): Char;
var
  AIsNegative: Boolean;
  AHasParameter: Boolean;
  AParameterValue: Integer;
begin
  if not ReadChar(AStream, Result) then
  begin
    Result := #0;
    Exit;
  end;
  if not IsAlpha(Result) then
  begin
    TranslateControlChar(Result);
    Result := ' ';
    Exit;
  end
  else
    FlushDecoder;
  FKeyword.Length := 0;
  while IsAlpha(Result) do
  begin
    FKeyword.Append(Result);
    ReadChar(AStream, Result);
  end;
  FParameterValueString.Length := 0;
  AIsNegative := Result = '-';
  if AIsNegative then
  begin
    FParameterValueString.Append(Result);
    ReadChar(AStream, Result);
  end;
  if AIsNegative and not IsDigit(Result) then
    FParameterValueString.Length := 0
  else
    while IsDigit(Result) do
    begin
      FParameterValueString.Append(Result);
      ReadChar(AStream, Result);
    end;
  AHasParameter := FParameterValueString.Length > 0;
  if AHasParameter then
    AParameterValue := StrToInt(FParameterValueString.ToString)
  else
    AParameterValue := 0;
  TranslateKeyword(FKeyword.ToString, AParameterValue, AHasParameter);
end;

procedure TdxRtfImporter.ParseUnicodeChar(AChar: Char);
begin
  FlushDecoder;
  Destination.ProcessChar(AChar);
  FSkipCount := Position.RtfFormattingInfo.UnicodeCharacterByteCount;
end;

function TdxRtfImporter.PopRtfState: TdxRichEditPopRtfState;
const
  ResultMap: array[Boolean] of TdxRichEditPopRtfState = (TdxRichEditPopRtfState.StackEmpty, TdxRichEditPopRtfState.StackNonEmpty);
var
  AOldDestination: TdxRichEditRtfDestinationBase;
begin
  AOldDestination := Destination;
  if StateManager.States.Count = OptionalGroupLevel then
    OptionalGroupLevel := MaxInt;
  StateManager.PopState;
  if StateManager.States.Count >= OptionalGroupLevel then
    StateManager.Destination := TdxSkipDestination.Create(Self);
  AOldDestination.AfterPopRtfState;
  if AOldDestination <> Destination then
    AOldDestination.Free;
  FSkipCount := 0;
  Result := ResultMap[StateManager.States.Count > 0];
end;

procedure TdxRtfImporter.SetCodePage(ACodePage: Cardinal);
begin
  Position.RtfFormattingInfo.CodePage := ACodePage;
end;

procedure TdxRtfImporter.SetFont(AFontInfo: TdxRtfFontInfo);
var
  ACodePage: Integer;
begin
  Position.SetFont(AFontInfo.Name);
  ACodePage := TdxEncoding.CodePageFromCharset(AFontInfo.Charset);
  SetCodePage(ACodePage);
end;

function TdxRtfImporter.GetCharacterStyleIndex(ARtfCharacterStyleIndex: Integer): Integer;
begin
  if CharacterStyleCollectionIndex.ContainsKey(ARtfCharacterStyleIndex) then
    Result := CharacterStyleCollectionIndex[ARtfCharacterStyleIndex]
  else
    Result := 0;
end;

function TdxRtfImporter.GetParagraphStyleIndex(ARtfParagraphStyleIndex: Integer): Integer;
begin
  if ParagraphStyleCollectionIndex.ContainsKey(ARtfParagraphStyleIndex) then
    Result := ParagraphStyleCollectionIndex[ARtfParagraphStyleIndex]
  else
    Result := 0;
end;

function TdxRtfImporter.GetStyleMergedCharacterProperties(ARtfStyleIndex: Integer): TdxMergedCharacterProperties;
var
  AStyleIndex: Integer;
  AMergedProperties: TdxMergedCharacterProperties;
begin
  AStyleIndex := GetCharacterStyleIndex(ARtfStyleIndex);
  AMergedProperties := DocumentModel.CharacterStyles[AStyleIndex].GetMergedCharacterProperties;
  try
    Result := GetStyleMergedCharacterProperties(AMergedProperties);
  finally
    AMergedProperties.Free;
  end;
end;

function TdxRtfImporter.GetStyleMergedCharacterProperties(AParentMergedProperties: TdxMergedCharacterProperties): TdxMergedCharacterProperties;
begin
  Result := TdxMergedCharacterProperties.Create(AParentMergedProperties);
  Result.Merge(DocumentModel.DefaultCharacterProperties);
end;

function TdxRtfImporter.GetStyleMergedParagraphCharacterProperties(ARtfCharacterStyleIndex, ARtfParagraphStyleIndex: Integer): TdxMergedCharacterProperties;
var
  AParagraphStyleIndex, ACharacterStyleIndex: Integer;
  AParagraphResult: TdxMergedCharacterProperties;
  ADefaultProperties: TdxCharacterProperties;
begin
  AParagraphStyleIndex := GetParagraphStyleIndex(ARtfParagraphStyleIndex);
  ACharacterStyleIndex := GetCharacterStyleIndex(ARtfCharacterStyleIndex);
  Result := DocumentModel.CharacterStyles[ACharacterStyleIndex].GetMergedCharacterProperties;
  AParagraphResult := DocumentModel.ParagraphStyles[AParagraphStyleIndex].GetMergedCharacterProperties;
  try
    Result.Merge(AParagraphResult);
  finally
    AParagraphResult.Free;
  end;
  ADefaultProperties := DocumentModel.DefaultCharacterProperties;
  Result.Merge(ADefaultProperties);
end;

function TdxRtfImporter.GetStyleMergedParagraphProperties(
  ARtfStyleIndex: Integer): TdxMergedParagraphProperties;
var
  AStyleIndex: Integer;
  AParagraphProperties: TdxMergedParagraphProperties;
begin
  AStyleIndex := GetParagraphStyleIndex(ARtfStyleIndex);
  AParagraphProperties := DocumentModel.ParagraphStyles[AStyleIndex].GetMergedParagraphProperties;
  try
    Result := GetStyleMergedParagraphProperties(AParagraphProperties);
  finally
    AParagraphProperties.Free;
  end;
end;

function TdxRtfImporter.GetStyleMergedParagraphProperties(
  AParentMergedProperties: TdxMergedParagraphProperties): TdxMergedParagraphProperties;
begin
  Result := TdxMergedParagraphProperties.Create(AParentMergedProperties);
  Result.Merge(DocumentModel.DefaultParagraphProperties);
end;

function TdxRtfImporter.GetStyleMergedTableCellProperties(
  AParentMergedProperties: TdxMergedTableCellProperties): TdxMergedTableCellProperties;
begin
  Result := TdxMergedTableCellProperties.Create(AParentMergedProperties);
  Result.Merge(DocumentModel.DefaultTableCellProperties);
end;

function TdxRtfImporter.GetStyleMergedTableRowProperties(
  AParentMergedProperties: TdxMergedTableRowProperties): TdxMergedTableRowProperties;
begin
  Result := TdxMergedTableRowProperties.Create(AParentMergedProperties);
  Result.Merge(DocumentModel.DefaultTableRowProperties);
end;

function TdxRtfImporter.GetTableStyleIndex(ARtfTableStyleIndex: Integer; AUnknowStyleIndex: Integer = 0): Integer;
begin
  if TableStyleCollectionIndex.ContainsKey(ARtfTableStyleIndex) then
    Result := TableStyleCollectionIndex[ARtfTableStyleIndex]
  else
    Result := AUnknowStyleIndex;
end;

procedure TdxRtfImporter.InsertParagraph;
var
  AInsertedParagraphIndex: TdxParagraphIndex;
  AOldFontName: string;
begin
  if Position.RtfFormattingInfo.Deleted and Options.IgnoreDeletedText then
    Exit;
  AInsertedParagraphIndex := Position.ParagraphIndex;
  Position.ParagraphFormattingInfo.LineSpacing := Position.ParagraphFormattingInfo.CalcLineSpacing(UnitConverter);
  Position.ParagraphFormattingInfo.LineSpacingType := Position.ParagraphFormattingInfo.CalcLineSpacingType;
  if not Position.UseLowAnsiCharactersFontName then
    PieceTable.InsertParagraphCore(Position)
  else
  begin
    AOldFontName := Position.CharacterFormatting.FontName;
    Position.CharacterFormatting.FontName := Position.LowAnsiCharactersFontName;
    PieceTable.InsertParagraphCore(Position);
    Position.CharacterFormatting.FontName := AOldFontName;
  end;
  ApplyParagraphFormatting(AInsertedParagraphIndex, False);
  ApplyFrameProperties(AInsertedParagraphIndex);
end;

procedure TdxRtfImporter.InsertSection;
var
  ATransaction: TdxHistoryTransaction;
  AParagraphIndex: TdxParagraphIndex;
begin
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AParagraphIndex := Position.ParagraphIndex;
    Position.ParagraphFormattingInfo.LineSpacing := Position.ParagraphFormattingInfo.CalcLineSpacing(UnitConverter);
    Position.ParagraphFormattingInfo.LineSpacingType := Position.ParagraphFormattingInfo.CalcLineSpacingType;
    PieceTable.InsertSectionParagraphCore(Position);
    ApplyParagraphFormatting(AParagraphIndex, True);
    DocumentModel.SafeEditor.PerformInsertSectionCore(AParagraphIndex);
  finally
    ATransaction.Free;
  end;
end;

procedure TdxRtfImporter.TranslateKeyword(const AKeyword: string;
  AParameterValue: Integer; AHasParameter: Boolean);
var
  AKeywordProcessed: Boolean;
begin
  if (FSkipCount = 0) or (AKeyword = 'bin') then
  begin
    AKeywordProcessed := Destination.ProcessKeyword(AKeyword, AParameterValue, AHasParameter);
    if AKeywordProcessed then
    begin
      if not (StateManager.Destination is TdxSkipDestination) then
        OptionalGroupLevel := MaxInt;
    end
    else
      if OptionalGroupLevel < MaxInt then
        StateManager.Destination := TdxSkipDestination.Create(Self);
  end
  else
    DecreaseSkipCount;
end;

function TdxRtfImporter.ApplyCellIndices(var ACellIndices: TArray<Integer>;
  AStartColumnIndex, AColumnCount, ACellIndex: Integer): Integer;
var
  I: Integer;
begin
  if AStartColumnIndex + AColumnCount > Length(ACellIndices) then
    ThrowInvalidFile;
  for I := AStartColumnIndex to AStartColumnIndex + AColumnCount - 1 do
    ACellIndices[I] := ACellIndex;
  Result := AStartColumnIndex + AColumnCount;
end;

procedure TdxRtfImporter.CheckTablesStructure;
var
  APieceTables: TdxFastList;
  APieceTable: TdxPieceTable;
  ATable: TdxTable;
  I, J: Integer;
begin
  APieceTables := DocumentModel.GetPieceTables(False);
  try
    for I := 0 to APieceTables.Count - 1 do
    begin
      APieceTable := APieceTables[I];
      for J := 0 to APieceTable.Tables.Count - 1 do
      begin
        ATable := APieceTable.Tables[J];
        CheckTableStructure(ATable);
      end;
    end;
  finally
    APieceTables.Free;
  end;
end;

procedure TdxRtfImporter.CheckTableStructure(ATable: TdxTable);
var
  ARows: TdxTableRowCollection;
  ARowCount: Integer;
  AFirstRow: TdxTableRow;
  ATotalColumnCount: Integer;
  ACells: TdxTableCellCollection;
  ACellCount: Integer;
  I: Integer;
  APrevRowCellIndices: TArray<Integer>;
begin
  ARows := ATable.Rows;
  ARowCount := ARows.Count;
  if ARowCount <= 1 then
    Exit;
  AFirstRow := ARows[0];
  ATotalColumnCount := AFirstRow.GridBefore + AFirstRow.GridAfter;
  ACells := AFirstRow.Cells;
  ACellCount := ACells.Count;
  for I := 0 to ACellCount -  1 do
    ATotalColumnCount := ATotalColumnCount + ACells[I].ColumnSpan;
  SetLength(APrevRowCellIndices, ATotalColumnCount);
  CalculateRowCellIndices(APrevRowCellIndices, AFirstRow);
  for I := 1 to ARowCount -  1 do
  begin
    CheckVerticalMerging(APrevRowCellIndices, ARows[I]);
    CalculateRowCellIndices(APrevRowCellIndices, ARows[I]);
  end;
end;

procedure TdxRtfImporter.CheckVerticalMerging(APrevRowCellIndices: TArray<Integer>; ATableRow: TdxTableRow);
var
  ACells: TdxTableCellCollection;
  ACellCount, ACurrentColumnIndex: Integer;
  I, J: Integer;
  AExpectedIndex: Integer;
  ALastColumnIndex: Integer;
begin
  ACells := ATableRow.Cells;
  ACellCount := ACells.Count;
  ACurrentColumnIndex := ATableRow.GridBefore;
  for I := 0 to ACellCount -  1 do
  begin
    if ACells[I].VerticalMerging = TdxMergingState.Continue then
    begin
      AExpectedIndex := APrevRowCellIndices[ACurrentColumnIndex];
      if (ACurrentColumnIndex > 0) and (APrevRowCellIndices[ACurrentColumnIndex - 1] = AExpectedIndex) then
        ThrowInvalidFile;
      ALastColumnIndex := ACurrentColumnIndex + ACells[I].ColumnSpan - 1;
      if (ALastColumnIndex + 1 < Length(APrevRowCellIndices)) and (APrevRowCellIndices[ALastColumnIndex + 1] = AExpectedIndex) then
        ThrowInvalidFile;
      for J := ACurrentColumnIndex + 1 to ALastColumnIndex do
        if APrevRowCellIndices[J] <> AExpectedIndex then
          ThrowInvalidFile;
    end;
    ACurrentColumnIndex := ACurrentColumnIndex + ACells[I].ColumnSpan;
  end;
end;

procedure TdxRtfImporter.CalculateRowCellIndices(var ACellIndices: TArray<Integer>; ARow: TdxTableRow);
var
  ACells: TdxTableCellCollection;
  ACellCount, AColumnIndex: Integer;
  I: Integer;
begin
  ACells := ARow.Cells;
  ACellCount := ACells.Count;
  AColumnIndex := ApplyCellIndices(ACellIndices, 0, ARow.GridBefore, 0);
  for I := 0 to ACellCount - 1 do
    AColumnIndex := ApplyCellIndices(ACellIndices, AColumnIndex, ACells[I].ColumnSpan, I + 1);
  ApplyCellIndices(ACellIndices, AColumnIndex, ARow.GridAfter, ACellCount + 1);
end;

function TdxRtfImporter.CheckSignature(AStream: TStream): Boolean;
var
  ASignature: array[0..4] of Byte;
  ABytesRead: Integer;
begin
  AStream.Position := 0;
  ABytesRead := AStream.Read(ASignature, Length(ASignature));
  AStream.Seek(-ABytesRead, soCurrent);
  Result := (ABytesRead = Length(ASignature)) and
    (ASignature[0] = Ord('{')) and
    (ASignature[1] = Ord('\')) and
    (ASignature[2] = Ord('r')) and
    (ASignature[3] = Ord('t')) and
    (ASignature[4] = Ord('f'));
  if not Result then
    ThrowInvalidRtfFile;
end;

procedure TdxRtfImporter.ClearNumberingList;
begin
  DocumentModel.NumberingLists.Clear;
end;

procedure TdxRtfImporter.DecreaseSkipCount;
begin
  FSkipCount := Max(0, FSkipCount - 1);
end;

procedure TdxRtfImporter.AddNumberingListToParagraph(AParagraph: TdxParagraph; AListIndex: TdxNumberingListIndex; ALevelIndex: Integer);
begin
  if not TdxDocumentFormatsHelper.ShouldInsertMultiLevelNumbering(DocumentModel) then
    ALevelIndex := 0;
  ConvertListLevelsToAnotherType(AListIndex);
  PieceTable.AddNumberingListToParagraph(AParagraph, AListIndex, ALevelIndex);
end;

procedure TdxRtfImporter.ApplyParagraphFormattingCore(AParagraph: TdxParagraph; AParagraphFormatting: TdxRtfParagraphFormattingInfo);
var
  AParagraphProperties: TdxParagraphProperties;
  AFormattingBase: TdxParagraphFormattingBase;
begin
  AParagraph.ParagraphStyleIndex := AParagraphFormatting.StyleIndex;
  AParagraphProperties := AParagraph.ParagraphProperties;
  Assert(not AParagraphProperties.IsUpdateLocked);
  if Assigned(FLastParagraphFormatting) and AParagraphFormatting.Equals(FLastParagraphFormatting) then
  begin
    AParagraphProperties.SetIndexInitial(FLastParagraphPropertiesCacheIndex);
  end
  else
  begin
    AFormattingBase := TdxParagraphFormattingBase.Create(PieceTable, DocumentModel,
      AParagraphFormatting, TdxParagraphFormattingOptions.RootParagraphFormattingOption);
    try
      AParagraphProperties.ReplaceInfo(AFormattingBase, AParagraphProperties.BatchUpdateChangeActions);
    finally
      AFormattingBase.Free;
    end;
    FreeAndNil(FLastParagraphFormatting);
    FLastParagraphFormatting := AParagraphFormatting.Clone as TdxRtfParagraphFormattingInfo;
    FLastParagraphPropertiesCacheIndex := AParagraphProperties.Index;
  end;
end;

procedure TdxRtfImporter.ApplyParagraphFrameFormatting(AParagraph: TdxParagraph;
  AParagraphFrameFormatting: TdxParagraphFrameFormattingInfo);
var
  AFrameProperties: TdxFrameProperties;
  AParagraphFrameFormattingCacheIndex: Integer;
  AFrameFormattingBase: TdxParagraphFrameFormattingBase;
begin
  AParagraph.CreateFrameProperties;
  AFrameProperties := AParagraph.FrameProperties;
  Assert(not AFrameProperties.IsUpdateLocked);
  AParagraphFrameFormattingCacheIndex := DocumentModel.Cache.ParagraphFrameFormattingInfoCache.GetItemIndex(AParagraphFrameFormatting);
  if AParagraphFrameFormattingCacheIndex = FLastParagraphFrameFormattingCacheIndex then
    AFrameProperties.SetIndexInitial(FLastParagraphFramePropertiesCacheIndex)
  else
  begin
    AFrameFormattingBase := TdxParagraphFrameFormattingBase.Create(PieceTable, DocumentModel,
      AParagraphFrameFormatting, TdxParagraphFrameFormattingOptions.RootParagraphFrameFormattingOption);
    try
      AFrameProperties.ReplaceInfo(AFrameFormattingBase,
        AFrameProperties.BatchUpdateChangeActions);
    finally
      AFrameFormattingBase.Free;
    end;
  end;
  FLastParagraphFrameFormattingCacheIndex := AParagraphFrameFormattingCacheIndex;
  FLastParagraphFramePropertiesCacheIndex := AFrameProperties.Index;
end;

procedure TdxRtfImporter.ConvertListLevelsToAnotherType(AListIndex: TdxNumberingListIndex);
var
  AList: TdxNumberingList;
  AType: TdxNumberingType;
begin
  AList := DocumentModel.NumberingLists[AListIndex];
  AType := TdxNumberingListHelper.GetListType(AList);
  if TdxDocumentFormatsHelper.NeedReplaceSimpleToBulletNumbering(DocumentModel) and (AType = TdxNumberingType.Simple) then
    ConvertListLevelsToAnotherTypeCore(AList.Levels, TdxCharacters.MiddleDot, 'Symbol', TdxNumberingFormat.Bullet)
  else
    if TdxDocumentFormatsHelper.NeedReplaceBulletedLevelsToDecimal(DocumentModel) and (AType = TdxNumberingType.Bullet) then
      ConvertListLevelsToAnotherTypeCore(AList.Levels, '%0:s.', DocumentModel.DefaultCharacterProperties.FontName, TdxNumberingFormat.Decimal);
end;

procedure TdxRtfImporter.ConvertListLevelsToAnotherTypeCore(const ALevels: TdxListLevelCollection; const ADisplayFormat, AFontName: string; AFormat: TdxNumberingFormat);
var
  I: Integer;
  ALevel: IdxListLevel;
begin
  for I := 0 to ALevels.Count - 1 do
  begin
    Supports(ALevels[I], IdxListLevel, ALevel);
    ALevel.ListLevelProperties.DisplayFormatString := ADisplayFormat;
    ALevel.CharacterProperties.FontName := AFontName;
    ALevel.ListLevelProperties.Format := AFormat;
  end;
end;

procedure TdxRtfImporter.LinkParagraphStyleWithNumberingLists(AStyle: TdxParagraphStyle);
var
  AInfo: TdxRtfNumberingListInfo;
  AIndex: TdxNumberingListIndex;
begin
  if not ParagraphStyleListOverrideIndexMap.TryGetValue(AStyle, AInfo) then
    Exit;
  if not ListOverrideIndexToNumberingListIndexMap.TryGetValue(AInfo.RtfNumberingListIndex, AIndex) then
    Exit;

  if (AIndex < 0) or (AIndex >= DocumentModel.NumberingLists.Count) then
    Exit;
  AStyle.SetNumberingListIndex(AIndex);
  AStyle.SetNumberingListLevelIndex(AInfo.ListLevelIndex);
end;

function TdxRtfImporter.GetUserNameById(AId: Integer): string;
var
  AIndex: Integer;
begin
  AIndex := AId - 1;
  if (AIndex < 0) or (AIndex + 1 > DocumentProperties.UserNames.Count) then
    Result := ''
  else
    Result := DocumentProperties.UserNames[AIndex];
end;

procedure TdxRtfImporter.FlushDecoder;
begin
  Position.RtfFormattingInfo.Decoder.Flush(Self);
end;

procedure TdxRtfImporter.LinkParagraphStylesWithNumberingLists;
var
  I: Integer;
  AStyles: TdxParagraphStyleCollection;
begin
  AStyles := DocumentModel.ParagraphStyles;
  for I := 0 to AStyles.Count - 1 do
    LinkParagraphStyleWithNumberingLists(AStyles[I]);
end;

procedure TdxRtfImporter.PushRtfState;
begin
  StateManager.PushState;
end;

class procedure TdxRtfImporter.ThrowInvalidFile;
begin
  ThrowInvalidRtfFile;
end;

class procedure TdxRtfImporter.ThrowInvalidRtfFile;
begin
  raise EdxRichEditArgumentException.Create('Invalid RTF file');
end;

class procedure TdxRtfImporter.ThrowUnexpectedEndOfFile;
begin
  raise EdxRichEditArgumentException.Create('Unexpected end of file');
end;

procedure TdxRtfImporter.TranslateControlChar(AChar: Char);
begin
  if (FSkipCount = 0) or (AChar = '''') then
    Destination.ProcessControlChar(AChar)
  else
    DecreaseSkipCount;
end;

procedure TdxRtfImporter.SkipDataBeyondOuterBrace(AStream: TStream);
begin
  AStream.Seek(0, soEnd);
end;

procedure TdxRtfImporter.ParseBinChar(AChar: Char);
begin
  Destination.ProcessBinChar(AChar);
  Dec(FBinCharCount);
  if FBinCharCount <= 0 then
  begin
    StateManager.ParsingState := TdxRichEditRtfParsingState.Normal;
    DecreaseSkipCount;
  end;
end;

procedure TdxRtfImporter.ParseChar(AChar: Char);
begin
  if FSkipCount = 0 then
    Position.RtfFormattingInfo.Decoder.ProcessChar(Self, AChar)
  else
    DecreaseSkipCount;
end;

procedure TdxRtfImporter.ParseHexChar(AStream: TStream; AChar: Char);
var
  AHex: Integer;
  ACh: Byte;
begin
  AHex := dxHexToInt(AChar) shl 4;
  ReadByte(AStream, ACh);
  AHex := AHex + dxHexToInt(Char(ACh));
  StateManager.ParsingState := TdxRichEditRtfParsingState.Normal;
  ParseChar(Char(AHex));
end;

function TdxRtfImporter.GetBookmarks: TdxImportBookmarkInfos;
begin
  Result := PieceTableInfo.Bookmarks;
end;

function TdxRtfImporter.GetRangePermissions: TdxImportRangePermissionInfos;
begin
  Result := PieceTableInfo.RangePermissions;
end;

function TdxRtfImporter.GetDestination: TdxRichEditRtfDestinationBase;
begin
  Result := StateManager.Destination;
end;

function TdxRtfImporter.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxRtfImporter.GetFields: TdxObjectStack<TdxRtfFieldInfo>;
begin
  Result := PieceTableInfo.Fields;
end;

function TdxRtfImporter.GetOptions: TdxRtfDocumentImporterOptions;
begin
  Result := TdxRtfDocumentImporterOptions(inherited Options);
end;

procedure TdxRtfImporter.SetBinCharCount(const Value: Integer);
begin
  if Value <= 0 then
    TdxRichEditExceptions.ThrowArgumentException('BinCharCount', Value);
  FBinCharCount := Value;
end;

function TdxRtfImporter.GetTableReader: TdxRtfTableReader;
begin
  Result := PieceTableInfo.TableReader;
end;

function TdxRtfImporter.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(PieceTableInfo.PieceTable);
end;

function TdxRtfImporter.GetPieceTableInfo: TdxRichEditRtfPieceTableInfo;
begin
  Result := StateManager.PieceTableInfo;
end;

function TdxRtfImporter.GetPosition: TdxRtfInputPosition;
begin
  Result := PieceTableInfo.Position;
end;

procedure TdxRtfImporter.SetDestination(const Value: TdxRichEditRtfDestinationBase);
begin
  StateManager.Destination := Value
end;

{ TdxRtfColorCollection }

function TdxRtfColorCollection.GetRtfColorById(AID: Integer): TdxAlphaColor;
begin
  if (AID < 0) or (AID >= Count) then
    Result := TdxAlphaColors.Empty
  else
    Result := Self[AID];
end;

{ TdxImportRtfFormat }

class function TdxImportRtfFormat.GetDocumentFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Rtf;
end;

function TdxImportRtfFormat.GetDocumentImporter: IdxImporter<TdxRichEditDocumentFormat, Boolean>;
begin
  Result := TdxRtfDocumentImporter.Create;
end;

function TdxImportRtfFormat.GetImporter(ADocumentModel: TdxCustomDocumentModel;
      const AOptions: IdxImporterOptions): TdxDocumentModelImporter;
begin
  Result := TdxRtfImporter.Create(ADocumentModel, AOptions);
end;

end.
