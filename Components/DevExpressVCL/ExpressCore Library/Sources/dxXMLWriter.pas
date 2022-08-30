{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCore Library                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxXMLWriter;

{$I cxVer.inc}
{$SCOPEDENUMS ON}

interface

uses
  Types, Classes, SysUtils, Generics.Defaults, Generics.Collections, Rtti,
  dxCore,
  dxGenerics,
  dxXMLClasses;

type
  TdxStringIntegerDictionary = class(TDictionary<string, Integer>);

  TdxXmlNewLineHandling = (
    Replace,
    Entitize,
    None
  );

  TdxXmlOutputMethod = (
    Xml,
    Html,
    Text,
    AutoDetect
  );

  TdxXmlStandalone = (
    Omit,
    Yes,
    No
  );

  TdxXmlNamespaceHandling = (
    Default,
    OmitDuplicates
  );

  TdxXmlWriteState = (
    Start,
    Prolog,
    Element,
    Attribute,
    Content,
    Closed,
    Error
  );

  TdxXmlSpace = (
    None,
    Default,
    Preserve
  );

  { TdxXmlWriterSettings }

  TdxXmlWriterSettings = class
  strict private
    FEncoding: TEncoding;
    FOmitXmlDeclaration: Boolean;
    FNewLineHandling: TdxXmlNewLineHandling;
    FNewLineChars: string;
    FIndent: Boolean;
    FIndentChars: string;
    FNewLineOnAttributes: Boolean;
    FNamespaceHandling: TdxXmlNamespaceHandling;
    FConformanceLevel: TdxXmlConformanceLevel;
    FEncodeInvalidXmlCharAsUCS2: Boolean;
    FCheckCharacters: Boolean;
    FWriteEndDocumentOnClose: Boolean;
    FOutputMethod: TdxXmlOutputMethod;
    FMergeCDataSections: Boolean;
    FStandalone: TdxXmlStandalone;
    FAutoXmlDeclaration: Boolean;

    procedure Initialize;
  public
    constructor Create;
    procedure Reset;

    property Encoding: TEncoding read FEncoding write FEncoding;
    property OmitXmlDeclaration: Boolean read FOmitXmlDeclaration write FOmitXmlDeclaration;
    property NewLineHandling: TdxXmlNewLineHandling read FNewLineHandling write FNewLineHandling;
    property NewLineChars: string read FNewLineChars write FNewLineChars;
    property Indent: Boolean read FIndent write FIndent;
    property IndentChars: string read FIndentChars write FIndentChars;
    property NewLineOnAttributes: Boolean read FNewLineOnAttributes write FNewLineOnAttributes;
    property ConformanceLevel: TdxXmlConformanceLevel read FConformanceLevel write FConformanceLevel;
    property CheckCharacters: Boolean read FCheckCharacters write FCheckCharacters;
    property NamespaceHandling: TdxXmlNamespaceHandling read FNamespaceHandling write FNamespaceHandling;
    property WriteEndDocumentOnClose: Boolean read FWriteEndDocumentOnClose write FWriteEndDocumentOnClose;
    property OutputMethod: TdxXmlOutputMethod read FOutputMethod write FOutputMethod;
    property MergeCDataSections: Boolean read FMergeCDataSections write FMergeCDataSections;
    property Standalone: TdxXmlStandalone read FStandalone write FStandalone;
    property AutoXmlDeclaration: Boolean read FAutoXmlDeclaration write FAutoXmlDeclaration;
    property EncodeInvalidXmlCharAsUCS2: Boolean read FEncodeInvalidXmlCharAsUCS2 write FEncodeInvalidXmlCharAsUCS2;
  end;

  { TdxXmlWriter }

  TdxXmlWriter = class abstract
  strict private
    FAttrEndPos: Integer;
    FContentPosition: Integer;
    FEncoding: TEncoding;
    FStream: TStream;
  protected
    constructor Create(AStream: TStream; AEncoding: TEncoding); overload;
    function GetWriteState: TdxXmlWriteState; virtual; abstract;
    function GetSettings: TdxXmlWriterSettings; virtual;
    function GetXmlSpace: TdxXmlSpace; virtual;
    function GetXmlLang: string; virtual;
    procedure Write(const ABuffer: string); overload; virtual;
    procedure Write(const ABuffer: TCharArray; AIndex, ALength: Integer); overload; virtual;

    property AttrEndPos: Integer read FAttrEndPos write FAttrEndPos;
    property ContentPosition: Integer read FContentPosition write FContentPosition;
    property Stream: TStream read FStream;
  public
    class function Create(AStream: TStream; ASettings: TdxXmlWriterSettings = nil): TdxXmlWriter; overload; static;
    class function Create(ASb: TStringBuilder; ASettings: TdxXmlWriterSettings = nil): TdxXmlWriter; overload; static;
    procedure WriteStartDocument; overload; virtual; abstract;
    procedure WriteStartDocument(AStandalone: Boolean); overload; virtual; abstract;
    procedure WriteEndDocument; virtual; abstract;
    procedure WriteStartElement(const ALocalName, ANs: string); overload;
    procedure WriteStartElement(APrefix: string; const ALocalName: string; ANs: string); overload; virtual;
    procedure WriteStartElement(const ALocalName: string); overload;
    procedure WriteEndElement; overload; virtual; abstract;
    procedure WriteFullEndElement; overload; virtual; abstract;
    procedure WriteAttributeBoolean(const ALocalName: string; AValue: Boolean); overload;
    procedure WriteAttributeFloat(const ALocalName: string; AValue: Single); overload;
    procedure WriteAttributeInteger(const ALocalName: string; AValue: Integer); overload;
    procedure WriteAttributeString(const ALocalName, ANs, AValue: string); overload;
    procedure WriteAttributeString(const ALocalName, AValue: string); overload;
    procedure WriteAttributeString(const APrefix, ALocalName, ANs, AValue: string); overload;
    procedure WriteAttributeString(const APrefix, ALocalName, ANs: string; const AValue: AnsiString); overload;
    procedure WriteAttributeString(const ALocalName: string; const AValue: AnsiString); overload;
    procedure WriteStartAttribute(const ALocalName, ANs: string); overload;
    procedure WriteStartAttribute(APrefix: string; ALocalName: string; ANs: string); overload; virtual; abstract;
    procedure WriteStartAttribute(const ALocalName: string); overload;
    procedure WriteEndAttribute; virtual;
    procedure WriteCData(const AText: string); virtual; abstract;
    procedure WriteComment(const AText: string); virtual; abstract;
    procedure WriteProcessingInstruction(const AName, AText: string); virtual; abstract;
    procedure WriteEntityRef(const AName: string); virtual; abstract;
    procedure WriteCharEntity(ACh: Char); virtual; abstract;
    procedure WriteWhitespace(const AWs: string); virtual; abstract;
    procedure WriteString(const AText: string); overload; virtual;
    procedure WriteString(const AText: AnsiString); overload; virtual;
    procedure WriteSurrogateCharEntity(ALowChar: Char; AHighChar: Char); virtual; abstract;
    procedure WriteChars(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer); virtual; abstract;
    procedure WriteRaw(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer); overload; virtual; abstract;
    procedure WriteRaw(const AData: string); overload; virtual; abstract;
    procedure WriteBase64(const ABuffer: TBytes; AIndex: Integer; ACount: Integer); virtual; abstract;
    procedure WriteBinHex(const ABuffer: TBytes; AIndex: Integer; ACount: Integer); virtual;
    procedure Close; virtual;
    procedure Flush; virtual;
    function LookupPrefix(const ANs: string): string; virtual; abstract;
    procedure WriteQualifiedName(const ALocalName, ANs: string); overload; virtual;
    procedure WriteValue(const AValue: string); overload; virtual;
    procedure WriteElementString(const ALocalName, AValue: string); overload;
    procedure WriteElementString(const ALocalName, ANs, AValue: string); overload;
    procedure WriteElementString(const APrefix, ALocalName, ANs, AValue: string); overload;
    procedure WriteElementString(const ALocalName: string; const AValue: AnsiString); overload;

    property Encoding: TEncoding read FEncoding;
    property Settings: TdxXmlWriterSettings read GetSettings;
    property WriteState: TdxXmlWriteState read GetWriteState;
    property XmlSpace: TdxXmlSpace read GetXmlSpace;
    property XmlLang: string read GetXmlLang;
  end;

implementation

uses
  Windows, Character,
  dxBase64,
  dxCharacters,
  dxStringHelper,
  dxXMLDoc;

resourcestring
  SXmlCanNotBindToReservedNamespace = 'Cannot bind to the reserved namespace.';
  SXmlCannotStartDocumentOnFragment = 'WriteStartDocument cannot be called on writers created with ConformanceLevel.Fragment.';
  SXmlCannotWriteXmlDecl = 'Cannot write XML declaration. XML declaration can be only at the beginning of the document.';
  SXmlClosedOrError = 'The Writer is closed or in error state.';
  SXmlConformanceLevelFragment = 'Make sure that the ConformanceLevel setting is set to ConformanceLevel.Fragment or ' +
    'ConformanceLevel.Auto if you want to write an XML fragment.';
  SXmlDupXmlDecl = 'Cannot write XML declaration. WriteStartDocument method has already written it.';
  SXmlDupAttributeName = '"%s" is a duplicate attribute name.';
  SXmlEmptyLocalName = 'The empty string is not a valid local name.';
  SXmlEmptyName = 'The empty string is not a valid name.';
  SXmlIndentCharsNotWhitespace = 'XmlWriterSettings.%s can contain only valid XML white space characters when ' +
    'XmlWriterSettings.CheckCharacters and XmlWriterSettings.NewLineOnAttributes are true.';
  SXmlInvalidCharacter = '%s, hexadecimal value %s, is an invalid character.';
  SXmlInvalidNameCharsDetail = 'Invalid name character in "%s". The %d character, hexadecimal value %s, cannot be included in a name.';
  SXmlInvalidCharsInIndent = 'WriterSettings.%s can contain only valid XML text content characters when XmlWriterSettings.CheckCharacters is true. %s';
  SXmlInvalidHighSurrogateChar = 'Invalid high surrogate character (%s). A high surrogate character must have a value from range (0xD800 - 0xDBFF).';
  SXmlInvalidOperation = 'Operation is not valid due to the current state of the object.';
  SXmlInvalidSurrogateMissingLowChar = 'The surrogate pair is invalid. Missing a low surrogate character.';
  SXmlInvalidXmlSpace = '"%s" is an invalid xml:space value.';
  SXmlNamespaceDeclXmlXmlns = 'Prefix "%s" cannot be mapped to namespace name reserved for "xml" or "xmlns".';
  SXmlNonWhitespace = 'Only white space characters should be used.';
  SXmlNoRoot = 'Document does not have a root element.';
  SXmlNoStartTag = 'There was no XML start tag open.';
  SXmlNotImplemented = 'Not implemented.';
  SXmlNotSupported = 'Not supported.';
  SXmlPrefixForEmptyNs = 'Cannot use a prefix with an empty namespace.';
  SXmlRedefinePrefix = 'The prefix "%s" cannot be redefined from "%s" to "%s" within the same start element tag.';
  SXmlUndefNamespace = 'The "%s" namespace is not defined.';
  SXmlWrongToken = 'Token ord=%d in state ord=%d would result in an invalid XML document.';
  SXmlXmlnsPrefix = 'Prefix "xmlns" is reserved for use by XML.';
  SXmlXmlPrefix = 'Prefix "xml" is reserved for use by XML and can be mapped only to namespace name "http://www.w3.org/XML/1998/namespace&quot"';

type
  TdxXmlRawWriter = class;

  { TdxXmlRawWriterBase64Encoder }

  TdxXmlRawWriterBase64Encoder = class(TdxBase64Encoder)
  strict private
    FRawWriter: TdxXmlRawWriter;
    procedure WriteChars(const AChars: TCharArray; AIndex: Integer; ACount: Integer); override;
  public
    constructor Create(ARawWriter: TdxXmlRawWriter);
  end;

  { TdxXmlRawWriter }

  TdxXmlRawWriter = class abstract (TdxXmlWriter)
  strict private
    FResolver: IdxXmlNamespaceResolver;
  strict protected
    FBase64Encoder: TdxXmlRawWriterBase64Encoder;
    function GetWriteState: TdxXmlWriteState; override;
    function GetXmlSpace: TdxXmlSpace; override;
    function GetXmlLang: string; override;
    function GetNamespaceResolver: IdxXmlNamespaceResolver; virtual;
    procedure SetNamespaceResolver(const AValue: IdxXmlNamespaceResolver); virtual;
    function GetSupportsNamespaceDeclarationInChunks: Boolean; virtual;
  public
    destructor Destroy; override;
    procedure WriteStartDocument; override;
    procedure WriteStartDocument(AStandalone: Boolean); override;
    procedure WriteEndDocument; override;
    procedure WriteEndElement; overload; override;
    procedure WriteFullEndElement; overload; override;
    procedure WriteBase64(const ABuffer: TBytes; AIndex: Integer; ACount: Integer); override;
    function LookupPrefix(const ANs: string): string; override;
    procedure WriteQualifiedName(const ALocalName, ANs: string); overload; override;
    procedure WriteCData(const AText: string); override;
    procedure WriteCharEntity(ACh: Char); override;
    procedure WriteSurrogateCharEntity(ALowChar: Char; AHighChar: Char); override;
    procedure WriteWhitespace(const AWs: string); override;
    procedure WriteChars(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer); override;
    procedure WriteRaw(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer); override;
    procedure WriteRaw(const AData: string); override;
    procedure WriteValue(const AValue: string); override;
    procedure WriteXmlDeclaration(AStandalone: TdxXmlStandalone); overload; virtual;
    procedure WriteXmlDeclaration(const AXmldecl: string); overload; virtual;
    procedure StartElementContent; virtual;
    procedure OnRootElement(AConformanceLevel: TdxXmlConformanceLevel); virtual;
    procedure WriteEndElement(const APrefix, ALocalName, ANs: string); overload; virtual;
    procedure WriteFullEndElement(const APrefix, ALocalName, ANs: string); overload; virtual;
    procedure WriteQualifiedName(const APrefix, ALocalName, ANs: string); overload; virtual;
    procedure WriteNamespaceDeclaration(const APrefix, ANs: string); virtual; abstract;
    procedure WriteStartNamespaceDeclaration(const APrefix: string); virtual;
    procedure WriteEndNamespaceDeclaration; virtual;
    procedure WriteEndBase64; virtual;

    property NamespaceResolver: IdxXmlNamespaceResolver read GetNamespaceResolver write SetNamespaceResolver;
    property SupportsNamespaceDeclarationInChunks: Boolean read GetSupportsNamespaceDeclarationInChunks;
  end;

  { TdxXmlEncodedRawTextWriter }

  TdxXmlEncodedRawTextWriter = class(TdxXmlRawWriter)
  public const
    BufferSize  = 1024 * 64;
    BufferOverflowSize = 32;
  strict private
    FBufPos: Integer;
    FTextPos: Integer;
    FContentPos: Integer;
    FCdataPos: Integer;
    FAttrEndPos: Integer;
    FBufLen: Integer;
    FWriteToNull: Boolean;
    FHadDoubleBracket: Boolean;
    FInAttributeValue: Boolean;
    FBufChars: TCharArray;
    FSettings: TdxXmlWriterSettings;
    FNewLineHandling: TdxXmlNewLineHandling;
    FOmitXmlDeclaration: Boolean;
    FNewLineChars: string;
    FCheckCharacters: Boolean;
    FEncodeInvalidXmlCharAsUCS2: Boolean;
    FStandalone: TdxXmlStandalone;
    FAutoXmlDeclaration: Boolean;
    FMergeCDataSections: Boolean;
  protected
    function GetSupportsNamespaceDeclarationInChunks: Boolean; override;
    procedure FlushBuffer; virtual;
    procedure WriteAttributeTextBlock(ASrc: PChar; ASrcEnd: PChar);
    procedure WriteElementTextBlock(ASrc: PChar; ASrcEnd: PChar);
    procedure RawText(const S: string); overload;
    procedure RawText(ASrcBegin: PChar; ASrcEnd: PChar); overload;
    procedure WriteRawWithCharChecking(APSrcBegin: PChar; APSrcEnd: PChar);
    procedure WriteCommentOrPi(const AText: string; AStopChar: Char);
    procedure WriteCDataSection(const AText: string);
    class function EncodeSurrogate(ASrc: PChar; ASrcEnd: PChar; ADst: PChar): PChar; static;
    function InvalidXmlChar(ACh: Char; ADst: PChar; AEntitize: Boolean): PChar;
    procedure EncodeChar(var ASrc: PChar; APSrcEnd: PChar; var ADst: PChar);
    function WriteNewLine(ADst: PChar): PChar;
    class function LtEntity(ADst: PChar): PChar; static;
    class function GtEntity(ADst: PChar): PChar; static;
    class function AmpEntity(ADst: PChar): PChar; static;
    class function QuoteEntity(ADst: PChar): PChar; static;
    class function TabEntity(ADst: PChar): PChar; static;
    class function LineFeedEntity(ADst: PChar): PChar; static;
    class function CarriageReturnEntity(ADst: PChar): PChar; static;
    class function CharEntity(ADst: PChar; ACh: Char): PChar; static;
    class function UCS2Entity(ADst: PChar; ACh: Char): PChar; static;
    class function RawStartCData(ADst: PChar): PChar; static;
    class function RawEndCData(ADst: PChar): PChar; static;
    procedure ValidateContentChars(const AChars: string; const APropertyName: string; AAllowOnlyWhitespace: Boolean);
  public
    constructor CreateEx(AStream: TStream; ASettings: TdxXmlWriterSettings); overload;
    destructor Destroy; override;

    procedure WriteXmlDeclaration(AStandalone: TdxXmlStandalone); override;
    procedure WriteXmlDeclaration(const AXmldecl: string); override;
    procedure WriteStartElement(APrefix: string; const ALocalName: string; ANs: string); override;
    procedure StartElementContent; override;
    procedure WriteEndElement(const APrefix, ALocalName, ANs: string); override;
    procedure WriteFullEndElement(const APrefix, ALocalName, ANs: string); override;
    procedure WriteStartAttribute(APrefix: string; ALocalName: string; ANs: string); override;
    procedure WriteEndAttribute; override;
    procedure WriteNamespaceDeclaration(const APrefix, ANamespaceName: string); override;
    procedure WriteStartNamespaceDeclaration(const APrefix: string); override;
    procedure WriteEndNamespaceDeclaration; override;
    procedure WriteCData(const AText: string); override;
    procedure WriteComment(const AText: string); override;
    procedure WriteProcessingInstruction(const AName, AText: string); override;
    procedure WriteEntityRef(const AName: string); override;
    procedure WriteCharEntity(C: Char); override;
    procedure WriteWhitespace(const AWhitespace: string); override;
    procedure WriteString(const AText: string); override;
    procedure WriteSurrogateCharEntity(ALowChar: Char; AHighChar: Char); override;
    procedure WriteChars(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer); override;
    procedure WriteRaw(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer); overload; override;
    procedure WriteRaw(const AData: string); overload; override;
    procedure Close; override;
    procedure Flush; override;
  end;

  { TdxXmlStringWriter }

  TdxXmlStringWriter = class(TdxXmlEncodedRawTextWriter)
  strict private
    FSb: TStringBuilder;
  protected
    procedure Write(const ABuffer: string); overload; override;
    procedure Write(const ABuffer: TCharArray; AIndex, ALength: Integer); overload; override;
  public
    constructor CreateEx(ASb: TStringBuilder; ASettings: TdxXmlWriterSettings); overload;
  end;

  { TdxXmlWellFormedWriter }

  TdxXmlWellFormedWriter = class(TdxXmlWriter)
  protected
  const
    ElementStackInitialSize = 8;
    NamespaceStackInitialSize = 8;
    AttributeArrayInitialSize = 8;
    MaxAttrDuplWalkCount = 14;
    MaxNamespacesWalkCount = 16;
  type
    TState = byte;
    TStates = class sealed
    public const
      Start = 0;
      TopLevel = 1;
      Document = 2;
      Element = 3;
      Content = 4;
      B64Content = 5;
      B64Attribute = 6;
      AfterRootEle = 7;
      Attribute = 8;
      SpecialAttr = 9;
      EndDocument = 10;
      RootLevelAttr = 11;
      RootLevelSpecAttr = 12;
      RootLevelB64Attr = 13;
      AfterRootLevelAttr = 14;
      Closed = 15;
      Error = 16;

      StartContent = 101;
      StartContentEle = 102;
      StartContentB64 = 103;
      StartDoc = 104;
      StartDocEle = 106;
      EndAttrSEle = 107;
      EndAttrEEle = 108;
      EndAttrSCont = 109;
      EndAttrSAttr = 111;
      PostB64Cont = 112;
      PostB64Attr = 113;
      PostB64RootAttr = 114;
      StartFragEle = 115;
      StartFragCont = 116;
      StartFragB64 = 117;
      StartRootLevelAttr = 118;
    end;

    TToken = (
      StartDocument,
      EndDocument,
      PI,
      Comment,
      Dtd,
      StartElement,
      EndElement,
      StartAttribute,
      EndAttribute,
      Text,
      CData,
      AtomicValue,
      Base64,
      RawData,
      Whitespace
    );

    TSpecialAttribute = (
      No,
      DefaultXmlns,
      PrefixedXmlns,
      XmlSpace,
      XmlLang
    );

    TNamespaceKind = (
      Written,
      NeedToWrite,
      Implied,
      Special
    );

    TElementScope = record
    strict private
      FPrevNSTop: Integer;
      FPrefix: string;
      FLocalName: string;
      FNamespaceUri: string;
      FXmlSpace: TdxXmlSpace;
      FXmlLang: string;
    public
      procedure &Set(const APrefix, ALocalName, ANamespaceUri: string; APrevNSTop: Integer);
      procedure WriteEndElement(ARawWriter: TdxXmlRawWriter);
      procedure WriteFullEndElement(ARawWriter: TdxXmlRawWriter);

      property PrevNSTop: Integer read FPrevNSTop;
      property XmlSpace: TdxXmlSpace read FXmlSpace write FXmlSpace;
      property XmlLang: string read FXmlLang write FXmlLang;
    end;

    TNamespace = record
    strict private
      FPrefix: string;
      FNamespaceUri: string;
      FKind: TNamespaceKind;
      FPrevNsIndex: Integer;
    public
      procedure &Set(const APrefix, ANamespaceUri: string; AKind: TNamespaceKind);
      procedure WriteDecl(AWriter: TdxXmlWriter; ARawWriter: TdxXmlRawWriter);

      property NamespaceUri: string read FNamespaceUri;
      property Kind: TNamespaceKind read FKind write FKind;
      property Prefix: string read FPrefix;
      property PrevNsIndex: Integer read FPrevNsIndex write FPrevNsIndex;
    end;

    TAttrName = record
    strict private
      FLocalName: string;
      FNamespaceUri: string;
      FPrefix: string;
      FPrev: Integer;
    public
      procedure &Set(const APrefix, ALocalName, ANamespaceUri: string);
      function IsDuplicate(const APrefix, ALocalName, ANamespaceUri: string): Boolean;

      property LocalName: string read FLocalName;
      property NamespaceUri: string read FNamespaceUri;
      property Prefix: string read FPrefix;
      property Prev: Integer read FPrev write FPrev;
    end;

    TAttributeValueCache = class
    strict private
    type
      TItemType = (
        EntityRef,
        CharEntity,
        SurrogateCharEntity,
        Whitespace,
        &String,
        StringChars,
        Raw,
        RawChars,
        ValueString
      );

      TItem = class
      strict private
        FType: TItemType;
        FData: TValue;
      public
        procedure &Set(AType: TItemType; const AData: TValue);

        property Data: TValue read FData write FData;
        property &Type: TItemType read FType;
      end;

      TBufferChunk = record
        Buffer: TCharArray;
        Index: Integer;
        Count: Integer;
        constructor Create(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer);
      end;

    strict private
      FStringValue: TStringBuilder;
      FSingleStringValue: string;
      FItems: TArray<TItem>;
      FFirstItem: Integer;
      FLastItem: Integer;
      function GetStringValue: string;
    public
      constructor Create;
      destructor Destroy; override;
      procedure WriteEntityRef(const AName: string);
      procedure WriteCharEntity(ACh: Char);
      procedure WriteSurrogateCharEntity(ALowChar: Char; AHighChar: Char);
      procedure WriteWhitespace(const AWs: string);
      procedure WriteString(const AText: string);
      procedure WriteChars(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer);
      procedure WriteRaw(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer); overload;
      procedure WriteRaw(const AData: string); overload;
      procedure WriteValue(const AValue: string);
      procedure Replay(AWriter: TdxXmlWriter);
      procedure Trim;
      procedure Clear;
      procedure StartComplexValue;
      procedure AddItem(AType: TItemType; AData: TValue);

      property StringValue: string read GetStringValue;
    end;

    TNamespaceResolverProxy = class(TInterfacedObject, IdxXmlNamespaceResolver)
    strict private
      FWfWriter: TdxXmlWellFormedWriter;
      function GetNamespacesInScope(AScope: TdxXmlNamespaceScope): TdxStringsDictionary;
      function LookupNamespace(const APrefix: string): string;
      function LookupPrefix(const ANamespaceName: string): string;
    public
      constructor Create(AWfWriter: TdxXmlWellFormedWriter);
    end;

  strict private
    class var
      FStateTableDocument: TArray<TState>;
      FStateTableAuto: TArray<TState>;
      FStateToWriteState: TArray<TdxXmlWriteState>;
  strict private
    FWriter: TdxXmlRawWriter;
    FPredefinedNamespaces: IdxXmlNamespaceResolver;
    FNsStack: TArray<TNamespace>;
    FNsTop: Integer;
    FNsHashTable: TdxStringIntegerDictionary;
    FUseNsHashTable: Boolean;
    FElemScopeStack: TArray<TElementScope>;
    FElemTop: Integer;
    FAttrStack: TArray<TAttrName>;
    FAttrCount: Integer;
    FAttrHashTable: TdxStringIntegerDictionary;
    FSpecAttr: TSpecialAttribute;
    FAttrValueCache: TAttributeValueCache;
    FCurDeclPrefix: string;
    FStateTable: TArray<TState>;
    FCurrentState: TState;
    FCheckCharacters: Boolean;
    FOmitDuplNamespaces: Boolean;
    FWriteEndDocumentOnClose: Boolean;
    FConformanceLevel: TdxXmlConformanceLevel;
    FXmlDeclFollows: Boolean;
    FHasher: TdxSecureStringHasher;
    class constructor Initialize;
  strict private
    function GetSaveAttrValue: Boolean;
    function GetInBase64: Boolean;
    function GetIsClosedOrErrorState: Boolean;
  protected
    function GetWriteState: TdxXmlWriteState; override;
    function GetSettings: TdxXmlWriterSettings; override;
    function GetXmlSpace: TdxXmlSpace; override;
    function GetXmlLang: string; override;
    procedure ThrowInvalidStateTransition(AToken: TToken; ACurrentState: TState);
  public
    constructor Create(AWriter: TdxXmlRawWriter; ASettings: TdxXmlWriterSettings);
    destructor Destroy; override;
    procedure WriteStartDocument; override;
    procedure WriteStartDocument(AStandalone: Boolean); override;
    procedure WriteEndDocument; override;
    procedure WriteStartElement(APrefix: string; const ALocalName: string; ANs: string); override;
    procedure WriteEndElement; override;
    procedure WriteFullEndElement; override;
    procedure WriteStartAttribute(APrefix: string; ALocalName: string; ANamespaceName: string); override;
    procedure WriteEndAttribute; override;
    procedure WriteCData(const AText: string); override;
    procedure WriteComment(const AText: string); override;
    procedure WriteProcessingInstruction(const AName, AText: string); override;
    procedure WriteEntityRef(const AName: string); override;
    procedure WriteCharEntity(ACh: Char); override;
    procedure WriteSurrogateCharEntity(ALowChar: Char; AHighChar: Char); override;
    procedure WriteWhitespace(const AWs: string); override;
    procedure WriteString(const AText: string); override;
    procedure WriteChars(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer); override;
    procedure WriteRaw(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer); override;
    procedure WriteRaw(const AData: string); override;
    procedure WriteBase64(const ABuffer: TBytes; AIndex: Integer; ACount: Integer); override;
    procedure Close; override;
    procedure Flush; override;
    function LookupPrefix(const ANs: string): string; override;
    procedure WriteQualifiedName(const ALocalName, ANs: string); override;
    procedure WriteValue(const AValue: string); override;
    procedure WriteBinHex(const ABuffer: TBytes; AIndex: Integer; ACount: Integer); override;
    procedure SetSpecialAttribute(ASpecial: TSpecialAttribute);
    procedure WriteStartDocumentImpl(AStandalone: TdxXmlStandalone);
    procedure StartFragment;
    procedure PushNamespaceImplicit(const APrefix, ANs: string);
    function PushNamespaceExplicit(const APrefix, ANs: string): Boolean;
    procedure AddNamespace(const APrefix, ANs: string; AKind: TNamespaceKind);
    procedure AddToNamespaceHashtable(ANamespaceIndex: Integer);
    function LookupNamespaceIndex(const APrefix: string): Integer;
    procedure PopNamespaces(AIndexFrom: Integer; AIndexTo: Integer);
    class function DupAttrException(const APrefix, ALocalName: string): EdxXmlException; static;
    procedure AdvanceState(AToken: TToken);
    procedure StartElementContent;
    function LookupNamespace(const APrefix: string): string;
    function LookupLocalNamespace(const APrefix: string): string;
    function GeneratePrefix: string;
    procedure CheckNCName(const ANcname: string);
    class function InvalidCharsException(const AName: string; ABadCharIndex: Integer): EdxXmlException; static;
    procedure AddAttribute(const APrefix, ALocalName, ANamespaceName: string);
    procedure AddToAttrHashTable(AAttributeIndex: Integer);

    property InnerWriter: TdxXmlRawWriter read FWriter;
    property SaveAttrValue: Boolean read GetSaveAttrValue;
    property InBase64: Boolean read GetInBase64;
    property IsClosedOrErrorState: Boolean read GetIsClosedOrErrorState;
  end;

function CharToHex(C: Char): string;
begin
  Result := Format('0x%.4x', [Ord(C)]);
end;

{ TdxXmlWriterSettings }

constructor TdxXmlWriterSettings.Create;
begin
  inherited Create;
  Initialize;
end;

procedure TdxXmlWriterSettings.Initialize;
begin
  FEncoding := TEncoding.UTF8;
  FOmitXmlDeclaration := False;
  FNewLineHandling := TdxXmlNewLineHandling.Replace;
  FNewLineChars := sLineBreak;
  FIndent := True;
  FIndentChars := '  ';
  FNewLineOnAttributes := False;
  FNamespaceHandling := TdxXmlNamespaceHandling.Default;
  FConformanceLevel := TdxXmlConformanceLevel.Document;
  FCheckCharacters := True;
  FWriteEndDocumentOnClose := True;

  FMergeCDataSections := False;
  FStandalone := TdxXmlStandalone.Omit;
end;

procedure TdxXmlWriterSettings.Reset;
begin
  Initialize;
end;

{$REGION 'TdxXmlWriter'}

{ TdxXmlWriter }

constructor TdxXmlWriter.Create(AStream: TStream; AEncoding: TEncoding);
begin
  inherited Create;
  FStream := AStream;
  FEncoding := AEncoding;
end;

class function TdxXmlWriter.Create(AStream: TStream; ASettings: TdxXmlWriterSettings = nil): TdxXmlWriter;
begin
  if ASettings = nil then
    ASettings := TdxXmlWriterSettings.Create;
  Result := TdxXmlWellFormedWriter.Create(TdxXmlEncodedRawTextWriter.CreateEx(AStream, ASettings), ASettings);
end;

class function TdxXmlWriter.Create(ASb: TStringBuilder; ASettings: TdxXmlWriterSettings = nil): TdxXmlWriter;
begin
  if ASettings = nil then
    ASettings := TdxXmlWriterSettings.Create;
  Result := TdxXmlWellFormedWriter.Create(TdxXmlStringWriter.CreateEx(ASb, ASettings), ASettings);
end;

procedure TdxXmlWriter.Flush;
begin
end;

function TdxXmlWriter.GetSettings: TdxXmlWriterSettings;
begin
  Result := nil;
end;

procedure TdxXmlWriter.WriteStartElement(const ALocalName, ANs: string);
begin
  WriteStartElement('', ALocalName, ANs);
end;

procedure TdxXmlWriter.WriteStartElement(const ALocalName: string);
begin
  WriteStartElement('', ALocalName, '');
end;

procedure TdxXmlWriter.WriteString(const AText: string);
begin
  Write(AText);
end;

procedure TdxXmlWriter.WriteString(const AText: AnsiString);
begin
  WriteString(dxAnsiStringToString(AText));
end;

procedure TdxXmlWriter.WriteAttributeBoolean(const ALocalName: string; AValue: Boolean);
begin
  WriteAttributeString(ALocalName, dxXMLStringToString(TdxXMLHelper.EncodeBoolean(AValue)));
end;

procedure TdxXmlWriter.WriteAttributeFloat(const ALocalName: string; AValue: Single);
begin
  WriteAttributeString(ALocalName, dxFloatToStr(AValue));
end;

procedure TdxXmlWriter.WriteAttributeInteger(const ALocalName: string; AValue: Integer);
begin
  WriteAttributeString(ALocalName, IntToStr(AValue));
end;

procedure TdxXmlWriter.WriteAttributeString(const ALocalName, ANs, AValue: string);
begin
  WriteStartAttribute('', ALocalName, ANs);
  WriteString(AValue);
  WriteEndAttribute;
end;

procedure TdxXmlWriter.WriteAttributeString(const ALocalName, AValue: string);
begin
  WriteStartAttribute('', ALocalName, '');
  WriteString(AValue);
  WriteEndAttribute;
end;

procedure TdxXmlWriter.WriteAttributeString(const APrefix, ALocalName, ANs, AValue: string);
begin
  WriteStartAttribute(APrefix, ALocalName, ANs);
  WriteString(AValue);
  WriteEndAttribute;
end;

procedure TdxXmlWriter.WriteAttributeString(const APrefix, ALocalName, ANs: string; const AValue: AnsiString);
begin
  WriteStartAttribute(APrefix, ALocalName, ANs);
  WriteString(AValue);
  WriteEndAttribute;
end;

procedure TdxXmlWriter.WriteAttributeString(const ALocalName: string; const AValue: AnsiString);
begin
  WriteStartAttribute(ALocalName);
  WriteString(AValue);
  WriteEndAttribute;
end;

procedure TdxXmlWriter.WriteStartAttribute(const ALocalName, ANs: string);
begin
  WriteStartAttribute('', ALocalName, ANs);
end;

procedure TdxXmlWriter.WriteStartAttribute(const ALocalName: string);
begin
  WriteStartAttribute('', ALocalName, '');
end;

procedure TdxXmlWriter.WriteStartElement(APrefix: string; const ALocalName: string; ANs: string);
begin
  if APrefix <> '' then
    Write('<' + APrefix + ':' + ALocalName)
  else
    Write('<' + ALocalName);
  FAttrEndPos := Stream.Position;
end;

procedure TdxXmlWriter.WriteBinHex(const ABuffer: TBytes; AIndex: Integer; ACount: Integer);
const
  CharsChunkSize = 128;
var
  AChars: TCharArray;
  AEndIndex, AChunkSize, ACharCount: Integer;
begin
  if ACount * 2 < CharsChunkSize then
    ACharCount := ACount * 2
  else
    ACharCount := CharsChunkSize;
  SetLength(AChars,  ACharCount);

  AEndIndex := AIndex + ACount;
  while AIndex < AEndIndex do
  begin
    if ACount < CharsChunkSize shr 1 then
      AChunkSize := ACount
    else
      AChunkSize := CharsChunkSize shr 1;
    ACharCount := TdxStringHelper.ToHex(ABuffer, AIndex, AChunkSize, AChars);
    WriteRaw(AChars, 0, ACharCount);
    Inc(AIndex, AChunkSize);
    Dec(ACount, AChunkSize);
  end;
end;

procedure TdxXmlWriter.Close;
begin
end;

function TdxXmlWriter.GetXmlSpace: TdxXmlSpace;
begin
  Result := TdxXmlSpace.Default;
end;

function TdxXmlWriter.GetXmlLang: string;
begin
  Result := '';
end;

procedure TdxXmlWriter.WriteQualifiedName(const ALocalName, ANs: string);
var
  APrefix: string;
begin
  if ANs <> '' then
  begin
    APrefix := LookupPrefix(ANs);
    if APrefix = '' then
      raise EdxXmlArgumentException.CreateFmt(SXmlUndefNamespace, [ANs]);
    WriteString(APrefix);
    WriteString(':');
  end;
  WriteString(ALocalName);
end;

procedure TdxXmlWriter.WriteValue(const AValue: string);
begin
  if AValue <> '' then
    WriteString(AValue);
end;

procedure TdxXmlWriter.WriteElementString(const ALocalName, AValue: string);
begin
  WriteElementString(ALocalName, '', AValue);
end;

procedure TdxXmlWriter.WriteElementString(const ALocalName, ANs, AValue: string);
begin
  WriteStartElement(ALocalName, ANs);
  if AValue <> '' then
    WriteString(AValue);
  WriteEndElement;
end;

procedure TdxXmlWriter.WriteElementString(const APrefix, ALocalName, ANs, AValue: string);
begin
  WriteStartElement(APrefix, ALocalName, ANs);
  if AValue <> '' then
    WriteString(AValue);
  WriteEndElement;
end;

procedure TdxXmlWriter.WriteElementString(const ALocalName: string; const AValue: AnsiString);
begin
  WriteStartElement(ALocalName);
  if AValue <> '' then
    WriteString(AValue);
  WriteEndElement;
end;

procedure TdxXmlWriter.WriteEndAttribute;
begin
  Write('"');
  FAttrEndPos := Stream.Position;
end;

procedure TdxXmlWriter.Write(const ABuffer: string);
var
  ABytes: TBytes;
begin
  ABytes := Encoding.GetBytes(ABuffer);
  Stream.WriteBuffer(ABytes[0], Length(ABytes));
end;

procedure TdxXmlWriter.Write(const ABuffer: TCharArray; AIndex, ALength: Integer);
var
  ABytes: TBytes;
begin
{$IFDEF DELPHIXE}
  ABytes := Encoding.GetBytes(ABuffer, AIndex, ALength);
{$ELSE}
  SetLength(ABytes, Encoding.GetByteCount(ABuffer, AIndex, ALength));
  Encoding.GetBytes(ABuffer, AIndex, ALength, ABytes, 0);
{$ENDIF}
  Stream.WriteBuffer(ABytes[0], Length(ABytes));
end;

{$ENDREGION 'TdxXmlWriter'}

{ TdxXmlRawWriterBase64Encoder }

constructor TdxXmlRawWriterBase64Encoder.Create(ARawWriter: TdxXmlRawWriter);
begin
  inherited Create;
  FRawWriter := ARawWriter;
end;

procedure TdxXmlRawWriterBase64Encoder.WriteChars(const AChars: TCharArray; AIndex: Integer; ACount: Integer);
begin
  FRawWriter.WriteRaw(AChars, AIndex, ACount);
end;

{$REGION 'TdxXmlRawWriter'}

{ TdxXmlRawWriter }

destructor TdxXmlRawWriter.Destroy;
begin
  FBase64Encoder.Free;
  inherited Destroy;
end;

procedure TdxXmlRawWriter.WriteStartDocument;
begin
  raise EdxXmlInvalidOperationException.Create(SXmlInvalidOperation);
end;

procedure TdxXmlRawWriter.WriteStartDocument(AStandalone: Boolean);
begin
  raise EdxXmlInvalidOperationException.Create(SXmlInvalidOperation);
end;

procedure TdxXmlRawWriter.WriteEndDocument;
begin
  raise EdxXmlInvalidOperationException.Create(SXmlInvalidOperation);
end;

procedure TdxXmlRawWriter.WriteEndElement(const APrefix, ALocalName, ANs: string);
begin
  if ContentPosition <> Stream.Position then
  begin
    if APrefix <> '' then
      Write('</' + APrefix + ':' + ALocalName + '>')
    else
      Write('</' + ALocalName + '>')
  end
  else
  begin
    Stream.Position := Stream.Position -1;
    Write(' />');
  end;
end;

procedure TdxXmlRawWriter.WriteEndElement;
begin
  raise EdxXmlInvalidOperationException.Create(SXmlInvalidOperation);
end;

procedure TdxXmlRawWriter.WriteFullEndElement;
begin
  raise EdxXmlInvalidOperationException.Create(SXmlInvalidOperation);
end;

procedure TdxXmlRawWriter.WriteBase64(const ABuffer: TBytes; AIndex: Integer; ACount: Integer);
begin
  if FBase64Encoder = nil then
    FBase64Encoder := TdxXmlRawWriterBase64Encoder.Create(Self);
  FBase64Encoder.Encode(ABuffer, AIndex, ACount);
end;

function TdxXmlRawWriter.LookupPrefix(const ANs: string): string;
begin
  raise EdxXmlInvalidOperationException.Create(SXmlInvalidOperation);
end;

function TdxXmlRawWriter.GetWriteState: TdxXmlWriteState;
begin
  raise EdxXmlInvalidOperationException.Create(SXmlInvalidOperation);
end;

function TdxXmlRawWriter.GetXmlSpace: TdxXmlSpace;
begin
  raise EdxXmlInvalidOperationException.Create(SXmlInvalidOperation);
end;

function TdxXmlRawWriter.GetXmlLang: string;
begin
  raise EdxXmlInvalidOperationException.Create(SXmlInvalidOperation);
end;

procedure TdxXmlRawWriter.WriteQualifiedName(const ALocalName, ANs: string);
begin
  raise EdxXmlInvalidOperationException.Create(SXmlInvalidOperation);
end;

procedure TdxXmlRawWriter.WriteCData(const AText: string);
begin
  WriteString(AText);
end;

procedure TdxXmlRawWriter.WriteCharEntity(ACh: Char);
begin
  WriteString(ACh);
end;

procedure TdxXmlRawWriter.WriteSurrogateCharEntity(ALowChar: Char; AHighChar: Char);
begin
  WriteString(ALowChar + AHighChar);
end;

procedure TdxXmlRawWriter.WriteWhitespace(const AWs: string);
begin
  WriteString(AWs);
end;

procedure TdxXmlRawWriter.WriteChars(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer);
begin
  WriteString(TdxStringHelper.Create(ABuffer, AIndex, ACount));
end;

procedure TdxXmlRawWriter.WriteRaw(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer);
begin
  WriteString(TdxStringHelper.Create(ABuffer, AIndex, ACount));
end;

procedure TdxXmlRawWriter.WriteRaw(const AData: string);
begin
  WriteString(AData);
end;

procedure TdxXmlRawWriter.WriteValue(const AValue: string);
begin
  WriteString(AValue);
end;

function TdxXmlRawWriter.GetNamespaceResolver: IdxXmlNamespaceResolver;
begin
  Result := FResolver;
end;

procedure TdxXmlRawWriter.SetNamespaceResolver(const AValue: IdxXmlNamespaceResolver);
begin
  FResolver := AValue;
end;

procedure TdxXmlRawWriter.StartElementContent;
begin
  Write('>');
  ContentPosition := Stream.Position;
end;

procedure TdxXmlRawWriter.WriteXmlDeclaration(AStandalone: TdxXmlStandalone);
begin
  Write('<?xml version="1.0" encoding="utf-8"?>');
end;

procedure TdxXmlRawWriter.WriteXmlDeclaration(const AXmldecl: string);
begin
end;

procedure TdxXmlRawWriter.OnRootElement(AConformanceLevel: TdxXmlConformanceLevel);
begin
end;

procedure TdxXmlRawWriter.WriteFullEndElement(const APrefix, ALocalName, ANs: string);
begin
  WriteEndElement(APrefix, ALocalName, ANs);
end;

procedure TdxXmlRawWriter.WriteQualifiedName(const APrefix, ALocalName, ANs: string);
begin
  if APrefix <> '' then
  begin
    WriteString(APrefix);
    WriteString(':');
  end;
  WriteString(ALocalName);
end;

function TdxXmlRawWriter.GetSupportsNamespaceDeclarationInChunks: Boolean;
begin
  Result := False;
end;

procedure TdxXmlRawWriter.WriteStartNamespaceDeclaration(const APrefix: string);
begin
  raise EdxXmlException.Create(SXmlNotSupported);
end;

procedure TdxXmlRawWriter.WriteEndNamespaceDeclaration;
begin
  raise EdxXmlException.Create(SXmlNotSupported);
end;

procedure TdxXmlRawWriter.WriteEndBase64;
begin
  FBase64Encoder.Flush;
end;
{$ENDREGION}

{$REGION 'TdxXmlEncodedRawTextWriter'}

{ TdxXmlEncodedRawTextWriter }

constructor TdxXmlEncodedRawTextWriter.CreateEx(AStream: TStream; ASettings: TdxXmlWriterSettings);
begin
  Assert(ASettings <> nil);
  inherited Create(AStream, ASettings.Encoding);
  FSettings := ASettings;
  FBufPos := 1;
  FTextPos := 1;
  FBufLen := BufferSize;

  FNewLineHandling := ASettings.NewLineHandling;
  FOmitXmlDeclaration := ASettings.OmitXmlDeclaration;
  FNewLineChars := ASettings.NewLineChars;
  FCheckCharacters := ASettings.CheckCharacters;
  FEncodeInvalidXmlCharAsUCS2 := ASettings.EncodeInvalidXmlCharAsUCS2;
  FStandalone := ASettings.Standalone;
  FMergeCDataSections := ASettings.MergeCDataSections;

  if FCheckCharacters and (FNewLineHandling = TdxXmlNewLineHandling.Replace) then
    ValidateContentChars(FNewLineChars, 'NewLineChars', False);

  SetLength(FBufChars, FBufLen + BufferOverflowSize);

  if ASettings.AutoXmlDeclaration then
  begin
    WriteXmlDeclaration(FStandalone);
    FAutoXmlDeclaration := True;
  end;
end;

destructor TdxXmlEncodedRawTextWriter.Destroy;
begin
  FreeAndNil(FSettings);
  inherited Destroy;
end;

procedure TdxXmlEncodedRawTextWriter.WriteXmlDeclaration(AStandalone: TdxXmlStandalone);
begin
  if not FOmitXmlDeclaration and not FAutoXmlDeclaration then
  begin
    RawText('<?xml version="');
    RawText('1.0');
    if Encoding <> nil then
    begin
      RawText('" encoding="');
      RawText('utf-8'{FEncoding.WebName});
    end;
    if AStandalone <> TdxXmlStandalone.Omit then
    begin
      RawText('" standalone="');
      if AStandalone = TdxXmlStandalone.Yes then
        RawText('yes')
      else
        RawText('no');
    end;
    RawText('"?>');
  end;
end;

procedure TdxXmlEncodedRawTextWriter.WriteXmlDeclaration(const AXmldecl: string);
begin
  if not FOmitXmlDeclaration and not FAutoXmlDeclaration then
    WriteProcessingInstruction('xml', AXmldecl);
end;

procedure TdxXmlEncodedRawTextWriter.WriteStartElement(APrefix: string; const ALocalName: string; ANs: string);
begin
  Assert(ALocalName <> '');

  FBufChars[FBufPos] := '<';
  Inc(FBufPos);
  if APrefix <> '' then
  begin
    RawText(APrefix);
    FBufChars[FBufPos] := ':';
    Inc(FBufPos);
  end;
  RawText(ALocalName);
  FAttrEndPos := FBufPos;
end;

procedure TdxXmlEncodedRawTextWriter.StartElementContent;
begin
  FBufChars[FBufPos] := '>';
  Inc(FBufPos);
  FContentPos := FBufPos;
end;

procedure TdxXmlEncodedRawTextWriter.WriteEndElement(const APrefix, ALocalName, ANs: string);
begin
  Assert(ALocalName <> '');

  if FContentPos <> FBufPos then
  begin
    FBufChars[FBufPos] := '<';
    Inc(FBufPos);
    FBufChars[FBufPos] := '/';
    Inc(FBufPos);

    if APrefix <> '' then
    begin
      RawText(APrefix);
      FBufChars[FBufPos] := ':';
      Inc(FBufPos);
    end;
    RawText(ALocalName);
    FBufChars[FBufPos] := '>';
    Inc(FBufPos);
  end
  else
  begin
    Dec(FBufPos);
    FBufChars[FBufPos] := ' ';
    Inc(FBufPos);
    FBufChars[FBufPos] := '/';
    Inc(FBufPos);
    FBufChars[FBufPos] := '>';
    Inc(FBufPos);
  end;
end;

procedure TdxXmlEncodedRawTextWriter.WriteFullEndElement(const APrefix, ALocalName, ANs: string);
begin
  Assert(ALocalName <> '');

  FBufChars[FBufPos] := '<';
  Inc(FBufPos);
  FBufChars[FBufPos] := '/';
  Inc(FBufPos);

  if APrefix <> '' then
  begin
    RawText(APrefix);
    FBufChars[FBufPos] := ':';
    Inc(FBufPos);
  end;
  RawText(ALocalName);
  FBufChars[FBufPos] := '>';
  Inc(FBufPos);
end;

procedure TdxXmlEncodedRawTextWriter.WriteStartAttribute(APrefix: string; ALocalName: string; ANs: string);
begin
  Assert(ALocalName <> '');

  if FAttrEndPos = FBufPos then
  begin
    FBufChars[FBufPos] := ' ';
    Inc(FBufPos);
  end;

  if APrefix <> '' then
  begin
    RawText(APrefix);
    FBufChars[FBufPos] := ':';
    Inc(FBufPos);
  end;
  RawText(ALocalName);
  FBufChars[FBufPos] := '=';
  Inc(FBufPos);
  FBufChars[FBufPos] := '"';
  Inc(FBufPos);

  FInAttributeValue := True;
end;

procedure TdxXmlEncodedRawTextWriter.WriteEndAttribute;
begin
  FBufChars[FBufPos] := '"';
  Inc(FBufPos);
  FInAttributeValue := False;
  FAttrEndPos := FBufPos;
end;

procedure TdxXmlEncodedRawTextWriter.WriteNamespaceDeclaration(const APrefix, ANamespaceName: string);
begin
  WriteStartNamespaceDeclaration(APrefix);
  WriteString(ANamespaceName);
  WriteEndNamespaceDeclaration;
end;

function TdxXmlEncodedRawTextWriter.GetSupportsNamespaceDeclarationInChunks: Boolean;
begin
  Result := True;
end;

procedure TdxXmlEncodedRawTextWriter.WriteStartNamespaceDeclaration(const APrefix: string);
begin
  if APrefix = '' then
    RawText(' xmlns="')
  else
  begin
    RawText(' xmlns:');
    RawText(APrefix);
    FBufChars[FBufPos] := '=';
    Inc(FBufPos);
    FBufChars[FBufPos] := '"';
    Inc(FBufPos);
  end;

  FInAttributeValue := True;
end;

procedure TdxXmlEncodedRawTextWriter.WriteEndNamespaceDeclaration;
begin
  FInAttributeValue := False;

  FBufChars[FBufPos] := '"';
  Inc(FBufPos);
  FAttrEndPos := FBufPos;
end;

procedure TdxXmlEncodedRawTextWriter.WriteCData(const AText: string);
begin
  Assert(AText <> '');

  if FMergeCDataSections and (FBufPos = FCdataPos) then
  begin
    Assert(FBufPos >= 4);
    Dec(FBufPos, 3);
  end
  else
  begin
    FBufChars[FBufPos] := '<';
    Inc(FBufPos);
    FBufChars[FBufPos] := '!';
    Inc(FBufPos);
    FBufChars[FBufPos] := '[';
    Inc(FBufPos);
    FBufChars[FBufPos] := 'C';
    Inc(FBufPos);
    FBufChars[FBufPos] := 'D';
    Inc(FBufPos);
    FBufChars[FBufPos] := 'A';
    Inc(FBufPos);
    FBufChars[FBufPos] := 'T';
    Inc(FBufPos);
    FBufChars[FBufPos] := 'A';
    Inc(FBufPos);
    FBufChars[FBufPos] := '[';
    Inc(FBufPos);
  end;

  WriteCDataSection(AText);

  FBufChars[FBufPos] := ']';
  Inc(FBufPos);
  FBufChars[FBufPos] := ']';
  Inc(FBufPos);
  FBufChars[FBufPos] := '>';
  Inc(FBufPos);

  FTextPos := FBufPos;
  FCdataPos := FBufPos;
end;

procedure TdxXmlEncodedRawTextWriter.WriteComment(const AText: string);
begin
  Assert(AText <> '');

  FBufChars[FBufPos] := '<';
  Inc(FBufPos);
  FBufChars[FBufPos] := '!';
  Inc(FBufPos);
  FBufChars[FBufPos] := '-';
  Inc(FBufPos);
  FBufChars[FBufPos] := '-';
  Inc(FBufPos);

  WriteCommentOrPi(AText, '-');

  FBufChars[FBufPos] := '-';
  Inc(FBufPos);
  FBufChars[FBufPos] := '-';
  Inc(FBufPos);
  FBufChars[FBufPos] := '>';
  Inc(FBufPos);
end;

procedure TdxXmlEncodedRawTextWriter.WriteProcessingInstruction(const AName, AText: string);
begin
  Assert(AName <> '');

  FBufChars[FBufPos] := '<';
  Inc(FBufPos);
  FBufChars[FBufPos] := '?';
  Inc(FBufPos);
  RawText(AName);

  if AText <> '' then
  begin
    FBufChars[FBufPos] := ' ';
    Inc(FBufPos);
    WriteCommentOrPi(AText, '?');
  end;

  FBufChars[FBufPos] := '?';
  Inc(FBufPos);
  FBufChars[FBufPos] := '>';
  Inc(FBufPos);
end;

procedure TdxXmlEncodedRawTextWriter.WriteEntityRef(const AName: string);
begin
  Assert(AName <> '');

  FBufChars[FBufPos] := '&';
  Inc(FBufPos);
  RawText(AName);
  FBufChars[FBufPos] := ';';
  Inc(FBufPos);

  if FBufPos > FBufLen then
    FlushBuffer;

  FTextPos := FBufPos;
end;

procedure TdxXmlEncodedRawTextWriter.WriteCharEntity(C: Char);
var
  AValue: string;
begin
  AValue := TdxStringHelper.ToHex(C);

  if FCheckCharacters and not TdxXmlCharType.IsCharData(C) then
    raise EdxXmlArgumentException.CreateFmt(SXmlInvalidCharacter, [C, CharToHex(C)]);

  FBufChars[FBufPos] := '&';
  Inc(FBufPos);
  FBufChars[FBufPos] := '#';
  Inc(FBufPos);
  FBufChars[FBufPos] := 'x';
  Inc(FBufPos);
  RawText(AValue);
  FBufChars[FBufPos] := ';';
  Inc(FBufPos);

  if FBufPos > FBufLen then
    FlushBuffer;

  FTextPos := FBufPos;
end;

procedure TdxXmlEncodedRawTextWriter.WriteWhitespace(const AWhitespace: string);
var
  AStart, AEnd: PChar;
begin
  Assert(AWhitespace <> '');

  AStart := PChar(AWhitespace);
  AEnd := AStart + Length(AWhitespace);
  if FInAttributeValue then
    WriteAttributeTextBlock(AStart, AEnd)
  else
    WriteElementTextBlock(AStart, AEnd);
end;

procedure TdxXmlEncodedRawTextWriter.WriteString(const AText: string);
var
  AStart, AEnd: PChar;
begin
  Assert(AText <> '');

  AStart := PChar(AText);
  AEnd := AStart + Length(AText);
  if FInAttributeValue then
    WriteAttributeTextBlock(AStart, AEnd)
  else
    WriteElementTextBlock(AStart, AEnd);
end;

procedure TdxXmlEncodedRawTextWriter.WriteSurrogateCharEntity(ALowChar: Char; AHighChar: Char);
var
  ASurrogateChar: Integer;
begin
  ASurrogateChar := TdxXmlCharType.CombineSurrogateChar(ALowChar, AHighChar);

  FBufChars[FBufPos] := '&';
  Inc(FBufPos);
  FBufChars[FBufPos] := '#';
  Inc(FBufPos);
  FBufChars[FBufPos] := 'x';
  Inc(FBufPos);
  RawText(TdxStringHelper.ToHex(Char(ASurrogateChar)));
  FBufChars[FBufPos] := ';';
  Inc(FBufPos);
  FTextPos := FBufPos;
end;

procedure TdxXmlEncodedRawTextWriter.WriteChars(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer);
var
  AStart: PChar;
begin
  Assert(ABuffer <> nil);
  Assert(AIndex >= 0);
  Assert((ACount >= 0) and (AIndex + ACount <= Length(ABuffer)));

  AStart := @ABuffer[AIndex];
  if FInAttributeValue then
    WriteAttributeTextBlock(AStart, AStart + ACount)
  else
    WriteElementTextBlock(AStart, AStart + ACount);
end;

procedure TdxXmlEncodedRawTextWriter.WriteRaw(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer);
var
  AStart: PChar;
begin
  Assert(ABuffer <> nil);
  Assert(AIndex >= 0);
  Assert((ACount >= 0) and (AIndex + ACount <= Length(ABuffer)));

  AStart := @ABuffer[AIndex];
  WriteRawWithCharChecking(AStart, AStart + ACount);
  FTextPos := FBufPos;
end;

procedure TdxXmlEncodedRawTextWriter.WriteRaw(const AData: string);
var
  AStart, AEnd: PChar;
begin
  Assert(AData <> '');

  AStart := PChar(AData);
  AEnd := AStart + Length(AData);
  WriteRawWithCharChecking(AStart, AEnd);
  FTextPos := FBufPos;
end;

procedure TdxXmlEncodedRawTextWriter.Close;
begin
  try
    FlushBuffer;
  finally
    FWriteToNull := True;
  end;
end;

procedure TdxXmlEncodedRawTextWriter.Flush;
begin
  FlushBuffer;
end;

procedure TdxXmlEncodedRawTextWriter.FlushBuffer;
begin
  try
    try
      if not FWriteToNull then
        Write(FBufChars, 1, FBufPos - 1);
    except
      FWriteToNull := True;
      raise;
    end;
  finally
    FBufChars[0] := FBufChars[FBufPos - 1];
    if FTextPos = FBufPos then
      FTextPos := 1
    else
      FTextPos := 0;
    if FAttrEndPos = FBufPos then
      FAttrEndPos := 1
    else
      FAttrEndPos := 0;
    FContentPos := 0;
    FCdataPos := 0;
    FBufPos := 1;
  end;
end;

procedure TdxXmlEncodedRawTextWriter.WriteAttributeTextBlock(ASrc: PChar; ASrcEnd: PChar);
var
  ADstBegin, ADstEnd, ADst: PChar;
  C: Char;
begin
  ADstBegin := @FBufChars[0];

  ADst := ADstBegin + FBufPos;

  C := #$0000;
  while True do
  begin
    ADstEnd := ADst + (ASrcEnd - ASrc);
    if ADstEnd > ADstBegin + FBufLen  then
      ADstEnd := ADstBegin + FBufLen;

    while ADst < ADstEnd do
    begin
      C := ASrc^;
      if (TdxXmlCharType.charProperties[C] and TdxXmlCharType.AttrValue) = 0 then
        Break;
      ADst^ := C;
      Inc(ADst);
      Inc(ASrc);
    end;
    Assert(ASrc <= ASrcEnd);

    if ASrc >= ASrcEnd then
      Break;
    if ADst >= ADstEnd then
    begin
      FBufPos := ADst - ADstBegin;
      FlushBuffer;
      ADst := ADstBegin + 1;
      Continue;
    end;
    case C of
      '&':
        ADst := AmpEntity(ADst);
      '<':
        ADst := LtEntity(ADst);
      '>':
        ADst := GtEntity(ADst);
      '"':
        ADst := QuoteEntity(ADst);
      #$0027:
        begin
          ADst^ := C;
          Inc(ADst);
        end;
      #$0009:
        begin
          if FNewLineHandling = TdxXmlNewLineHandling.None then
          begin
            ADst^ := C;
            Inc(ADst);
          end
          else
            ADst := TabEntity(ADst);
        end;
      #$000D:
        begin
          if FNewLineHandling = TdxXmlNewLineHandling.None then
          begin
            ADst^ := C;
            Inc(ADst);
          end
          else
            ADst := CarriageReturnEntity(ADst);
        end;
      #$000A:
        begin
          if FNewLineHandling = TdxXmlNewLineHandling.None then
          begin
            ADst^ := C;
            Inc(ADst);
          end
          else
            ADst := LineFeedEntity(ADst);
        end;
      else
      begin
        if TdxXmlCharType.IsSurrogate(C) then
        begin
          ADst := EncodeSurrogate(ASrc, ASrcEnd, ADst);
          Inc(ASrc, 2);
        end
        else
          if (C <= #$007F) or (C >= #$FFFE) then
          begin
            ADst := InvalidXmlChar(C, ADst, True);
            Inc(ASrc);
          end
          else
          begin
            ADst^ := C;
            Inc(ADst);
            Inc(ASrc);
          end;
        Continue;
      end;
    end;
    Inc(ASrc);
  end;
  FBufPos := ADst - ADstBegin;
end;

procedure TdxXmlEncodedRawTextWriter.WriteElementTextBlock(ASrc: PChar; ASrcEnd: PChar);
var
  ADst, ADstEnd, ADstBegin: PChar;
  C: Char;
begin
  ADstBegin := @FBufChars[0];
  ADst := ADstBegin + FBufPos;

  C := #$0000;
  while True do
  begin
    ADstEnd := ADst + (ASrcEnd - ASrc);
    if ADstEnd > ADstBegin + FBufLen then
      ADstEnd := ADstBegin + FBufLen;

    while ADst < ADstEnd do
    begin
      C := ASrc^;
      if (TdxXmlCharType.CharProperties[C] and TdxXmlCharType.AttrValue) = 0 then
        Break;
      ADst^ := C;
      Inc(ADst);
      Inc(ASrc);
    end;
    Assert(ASrc <= ASrcEnd);
    if ASrc >= ASrcEnd then
      Break;
    if ADst >= ADstEnd then
    begin
      FBufPos := Integer((ADst - ADstBegin));
      FlushBuffer;
      ADst := ADstBegin + 1;
      Continue;
    end;
    case C of
      '&':
        ADst := AmpEntity(ADst);
      '<':
        ADst := LtEntity(ADst);
      '>':
        ADst := GtEntity(ADst);
      '"',
      #$0027, #$0009:
        begin
          ADst^ := C;
          Inc(ADst);
        end;
      #$000A:
        begin
          if FNewLineHandling = TdxXmlNewLineHandling.Replace then
            ADst := WriteNewLine(ADst)
          else
          begin
            ADst^ := C;
            Inc(ADst);
          end;
        end;
      #$000D:
        begin
          case FNewLineHandling of
            TdxXmlNewLineHandling.Replace:
              begin
                if ASrc[1] = #$000A then
                  Inc(ASrc);
                ADst := WriteNewLine(ADst);
              end;
            TdxXmlNewLineHandling.Entitize:
              ADst := CarriageReturnEntity(ADst);
            TdxXmlNewLineHandling.None:
              begin
                ADst^ := C;
                Inc(ADst);
              end;
          end;
        end;
    else
    begin
      if TdxXmlCharType.IsSurrogate(C) then
      begin
        ADst := EncodeSurrogate(ASrc, ASrcEnd, ADst);
        Inc(ASrc, 2);
      end
      else
        if (C <= #$007F) or (C >= #$FFFE) then
        begin
          ADst := InvalidXmlChar(C, ADst, True);
          Inc(ASrc);
        end
        else
        begin
          ADst^ := C;
          Inc(ADst);
          Inc(ASrc);
        end;
      end;
      Continue;
    end;
    Inc(ASrc);
  end;
  FBufPos := (ADst - ADstBegin);
  FTextPos := FBufPos;
  FContentPos := 0;
end;

procedure TdxXmlEncodedRawTextWriter.RawText(const S: string);
var
  AStart, AEnd: PChar;
begin
  Assert(S <> '');
  AStart := PChar(S);
  AEnd := AStart + Length(S);
  RawText(AStart, AEnd);
end;

procedure TdxXmlEncodedRawTextWriter.RawText(ASrcBegin: PChar; ASrcEnd: PChar);
var
  ADstBegin, ADst, ASrc, ADstEnd: PChar;
  C: Char;
begin
  ADstBegin := @FBufChars[0];
  ADst := ADstBegin + FBufPos;
  ASrc := ASrcBegin;
  C := #$0000;
  while True do
  begin
    ADstEnd := ADst + (ASrcEnd - ASrc);
    if ADstEnd > ADstBegin + FBufLen then
      ADstEnd := ADstBegin + FBufLen;

    while ADst < ADstEnd do
    begin
      C := ASrc^;
      if C >= TdxXmlCharType.SurHighStart then
        Break;
      Inc(ASrc);
      ADst^ := C;
      Inc(ADst);
    end;
    Assert(ASrc <= ASrcEnd);
    if ASrc >= ASrcEnd then
      Break;
    if ADst >= ADstEnd then
    begin
      FBufPos := ADst - ADstBegin;
      FlushBuffer;
      ADst := ADstBegin + 1;
      Continue;
    end;

    if TdxXmlCharType.IsSurrogate(C) then
    begin
      ADst := EncodeSurrogate(ASrc, ASrcEnd, ADst);
      Inc(ASrc, 2);
    end
    else
      if (C <= #$007F) or (C >= #$FFFE) then
      begin
        ADst := InvalidXmlChar(C, ADst, False);
        Inc(ASrc);
      end
      else
      begin
        ADst^ := C;
        Inc(ADst);
        Inc(ASrc);
      end;
  end;
  FBufPos := ADst - ADstBegin;
end;

procedure TdxXmlEncodedRawTextWriter.WriteRawWithCharChecking(APSrcBegin: PChar; APSrcEnd: PChar);
var
  ADstBegin, ASrc, ADst, ADstEnd: PChar;
  C: Char;
begin
  ADstBegin := @FBufChars[0];
  ASrc := APSrcBegin;
  ADst := ADstBegin + FBufPos;

  C := #0000;
  while True do
  begin
    ADstEnd := ADst + (APSrcEnd - ASrc);
    if ADstEnd > ADstBegin + FBufLen then
      ADstEnd := ADstBegin + FBufLen;

    while ADst < ADstEnd do
    begin
      C := ASrc^;
      if not ((TdxXmlCharType.CharProperties[C] and TdxXmlCharType.Text) <> 0) then
        Break;
      ADst^ := C;
      Inc(ADst);
      Inc(ASrc);
    end;

    Assert(ASrc <= APSrcEnd);

    if ASrc >= APSrcEnd then
      Break;
    if ADst >= ADstEnd then
    begin
      FBufPos := ADst - ADstBegin;
      FlushBuffer;
      ADst := ADstBegin + 1;
      Continue;
    end;
    case C of
      ']', '<', '&', #$0009:
        begin
          ADst^ := C;
          Inc(ADst);
        end;
      #$000D:
        if FNewLineHandling = TdxXmlNewLineHandling.Replace then
        begin
          if ASrc[1] = #10 then
            Inc(ASrc);
          ADst := WriteNewLine(ADst);
        end
        else
        begin
          ADst^ := C;
          Inc(ADst);
        end;
      #$000A:
        if FNewLineHandling = TdxXmlNewLineHandling.Replace then
          ADst := WriteNewLine(ADst)
        else
        begin
          ADst^ := C;
          Inc(ADst);
        end;
      else
      begin
        if TdxXmlCharType.IsSurrogate(C) then
        begin
          ADst := EncodeSurrogate(ASrc, APSrcEnd, ADst);
          Inc(ASrc, 2);
        end
        else
          if (C <= #$007F) or (C >= #$FFFE) then
          begin
            ADst := InvalidXmlChar(C, ADst, False);
            Inc(ASrc);
          end
          else
          begin
            ADst^ := C;
            Inc(ADst);
            Inc(ASrc);
          end;
        Continue;
      end;
    end;
    Inc(ASrc);
  end;
  FBufPos := ADst - ADstBegin;
end;

procedure TdxXmlEncodedRawTextWriter.WriteCommentOrPi(const AText: string; AStopChar: Char);
var
  ASrcBegin, ADstBegin, ASrc, ASrcEnd, ADst, ADstEnd: PChar;
  C: Char;
begin
  if AText = '' then
  begin
    if FBufPos >= FBufLen then
      FlushBuffer;
    Exit;
  end;

  ASrcBegin := PChar(AText);
  ADstBegin := @FBufChars[0];
  ASrc := ASrcBegin;
  ASrcEnd := ASrcBegin + Length(AText);
  ADst := ADstBegin + FBufPos;
  C := #$0000;
  while True do
  begin
    ADstEnd := ADst + (ASrcEnd - ASrc);
    if ADstEnd > ADstBegin + FBufLen then
      ADstEnd := ADstBegin + FBufLen;

    while ADst < ADstEnd do
    begin
      C := ASrc^;
      if not (((TdxXmlCharType.CharProperties[C] and TdxXmlCharType.Text) <> 0) and (C <> AStopChar)) then
        Break;
      ADst^ := C;
      Inc(ADst);
      Inc(ASrc);
    end;

    Assert(ASrc <= ASrcEnd);
    if ASrc >= ASrcEnd then
      Break;
    if ADst >= ADstEnd then
    begin
      FBufPos := ADst - ADstBegin;
      FlushBuffer;
      ADst := ADstBegin + 1;
      Continue;
    end;

    case C of
      '-':
        begin
          ADst^ := '-';
          Inc(ADst);
          if C = AStopChar then
          begin
            if (ASrc + 1 = ASrcEnd) or (ASrc[1] = '-') then
            begin
              ADst^ := ' ';
              Inc(ADst);
            end;
          end;
        end;
      '?':
        begin
          ADst^ := '?';
          Inc(ADst);
          if C = AStopChar then
          begin
            if (ASrc + 1 < ASrcEnd) and (ASrc[1] = '>') then
            begin
              ADst^ := ' ';
              Inc(ADst);
            end;
          end;
        end;
      ']':
        begin
          ADst^ := ']';
          Inc(ADst);
        end;
      #$000D:
        if FNewLineHandling = TdxXmlNewLineHandling.Replace then
        begin
          if ASrc[1] = #$000A then
            Inc(ASrc);
          ADst := WriteNewLine(ADst);
        end
        else
        begin
          ADst^ := C;
          Inc(ADst);
        end;
      #$000A:
        if FNewLineHandling = TdxXmlNewLineHandling.Replace then
          ADst := WriteNewLine(ADst)
        else
        begin
          ADst^ := C;
          Inc(ADst);
        end;
      '<', '&', #$0009:
        begin
          ADst^ := C;
          Inc(ADst);
        end;
      else
      begin
        if TdxXmlCharType.IsSurrogate(C) then
        begin
          ADst := EncodeSurrogate(ASrc, ASrcEnd, ADst);
          Inc(ASrc, 2);
        end
        else
          if (C <= #$007F) or (C >= #$FFFE) then
          begin
            ADst := InvalidXmlChar(C, ADst, False);
            Inc(ASrc);
          end
          else
          begin
            ADst^ := C;
            Inc(ADst);
            Inc(ASrc);
          end;
        Continue;
      end;
    end;
    Inc(ASrc);
  end;
  FBufPos := ADst - ADstBegin;
end;

procedure TdxXmlEncodedRawTextWriter.WriteCDataSection(const AText: string);
var
  ASrcBegin, ADstBegin, ASrc, ASrcEnd, ADst, ADstEnd: PChar;
  C: Char;
begin
  if AText = '' then
  begin
    if FBufPos >= FBufLen then
      FlushBuffer;
    Exit;
  end;

  ASrcBegin := PChar(AText);
  ADstBegin := @FBufChars[0];
  ASrc := ASrcBegin;
  ASrcEnd := ASrcBegin + Length(AText);
  ADst := ADstBegin + FBufPos;

  C := #$000;
  while True do
  begin
    ADstEnd := ADst + (ASrcEnd - ASrc);
    if ADstEnd > ADstBegin + FBufLen then
      ADstEnd := ADstBegin + FBufLen;

    while ADst < ADstEnd do
    begin
      C := ASrc^;
      if not (((TdxXmlCharType.CharProperties[C] and TdxXmlCharType.AttrValue) <> 0) and (C <> ']')) then
        Break;
      ADst^ := C;
      Inc(ADst);
      Inc(ASrc);
    end;

    Assert(ASrc <= ASrcEnd);
    if ASrc >= ASrcEnd then
      Break;
    if ADst >= ADstEnd then
    begin
      FBufPos := ADst - ADstBegin;
      FlushBuffer;
      ADst := ADstBegin + 1;
      Continue;
    end;
    case C of
      '>':
        begin
          if FHadDoubleBracket and (ADst[-1] = ']') then
          begin
            ADst := RawEndCData(ADst);
            ADst := RawStartCData(ADst);
          end;
          ADst^ := '>';
          Inc(ADst);
        end;
      ']':
        begin
          FHadDoubleBracket := ADst[-1] = ']';
          ADst^ := ']';
          Inc(ADst);
        end;
      #$000D:
        if FNewLineHandling = TdxXmlNewLineHandling.Replace then
        begin
          if ASrc[1] = #$000A then
            Inc(ASrc);
          ADst := WriteNewLine(ADst);
        end
        else
        begin
          ADst^ := C;
          Inc(ADst);
        end;
      #$000A:
        if FNewLineHandling = TdxXmlNewLineHandling.Replace then
          ADst := WriteNewLine(ADst)
        else
        begin
          ADst^ := C;
          Inc(ADst);
        end;
      '&', '<', '"', #$0027, #$0009:
        begin
          ADst^ := C;
          Inc(ADst);
        end;
      else
      begin
        if TdxXmlCharType.IsSurrogate(C) then
        begin
          ADst := EncodeSurrogate(ASrc, ASrcEnd, ADst);
          Inc(ASrc, 2);
        end
        else
          if (C <= #$007F) or (C >= #$FFFE) then
          begin
            ADst := InvalidXmlChar(C, ADst, False);
            Inc(ASrc);
          end
          else
          begin
            ADst^ := C;
            Inc(ADst);
            Inc(ASrc);
          end;
        Continue;
      end;
    end;
    Inc(ASrc);
  end;
  FBufPos := ADst - ADstBegin;
end;

class function TdxXmlEncodedRawTextWriter.EncodeSurrogate(ASrc: PChar; ASrcEnd: PChar; ADst: PChar): PChar;
var
  ACh, ALowChar: Char;
begin
  ACh := ASrc^;
  Assert(TdxXmlCharType.IsSurrogate(ACh));
  if ACh <= TdxXmlCharType.SurHighEnd then
  begin
    if ASrc + 1 < ASrcEnd then
    begin
      ALowChar := ASrc[1];
      if ALowChar >= TdxXmlCharType.SurLowStart then
      begin
        ADst[0] := ACh;
        ADst[1] := ALowChar;
        Inc(ADst, 2);
        Exit(ADst);
      end;
      raise CreateInvalidSurrogatePairException(ALowChar, ACh);
    end;
    raise EdxXmlArgumentException.Create(SXmlInvalidSurrogateMissingLowChar);
  end;
  raise EdxXmlArgumentException.CreateFmt(SXmlInvalidHighSurrogateChar, [CharToHex(ACh)]);
end;

function TdxXmlEncodedRawTextWriter.InvalidXmlChar(ACh: Char; ADst: PChar; AEntitize: Boolean): PChar;
begin
  Assert(not TdxXmlCharType.IsWhiteSpace(ACh));
  Assert(not TdxXmlCharType.IsAttributeValueChar(ACh));

  if FCheckCharacters then
    raise EdxXmlArgumentException.CreateFmt(SXmlInvalidCharacter, [ACh, CharToHex(ACh)]);

  if AEntitize then
  begin
    if FEncodeInvalidXmlCharAsUCS2 then
      Result := UCS2Entity(ADst, ACh)
    else
      Result := CharEntity(ADst, ACh);
  end
  else
  begin
    ADst^ := ACh;
    Inc(ADst);
    Result := ADst;
  end;
end;

procedure TdxXmlEncodedRawTextWriter.EncodeChar(var ASrc: PChar; APSrcEnd: PChar; var ADst: PChar);
var
  C: Char;
begin
  C := ASrc^;
  if TdxXmlCharType.IsSurrogate(C) then
  begin
    ADst := EncodeSurrogate(ASrc, APSrcEnd, ADst);
    Inc(ASrc, 2);
  end
  else
    if (C <= #$007F) or (C >= #$FFFE) then
    begin
      ADst := InvalidXmlChar(C, ADst, False);
      Inc(ASrc);
    end
    else
    begin
      ADst^ := C;
      Inc(ADst);
      Inc(ASrc);
    end;
end;

function TdxXmlEncodedRawTextWriter.WriteNewLine(ADst: PChar): PChar;
var
  ADstBegin: PChar;
begin
  ADstBegin := @FBufChars[0];
  FBufPos := ADst - ADstBegin;
  RawText(FNewLineChars);
  Result := ADstBegin + FBufPos;
end;


class function TdxXmlEncodedRawTextWriter.LtEntity(ADst: PChar): PChar;
begin
  ADst[0] := '&';
  ADst[1] := 'l';
  ADst[2] := 't';
  ADst[3] := ';';
  Result := ADst + 4;
end;

class function TdxXmlEncodedRawTextWriter.GtEntity(ADst: PChar): PChar;
begin
  ADst[0] := '&';
  ADst[1] := 'g';
  ADst[2] := 't';
  ADst[3] := ';';
  Result := ADst + 4;
end;

class function TdxXmlEncodedRawTextWriter.AmpEntity(ADst: PChar): PChar;
begin
  ADst[0] := '&';
  ADst[1] := 'a';
  ADst[2] := 'm';
  ADst[3] := 'p';
  ADst[4] := ';';
  Result := ADst + 5;
end;

class function TdxXmlEncodedRawTextWriter.QuoteEntity(ADst: PChar): PChar;
begin
  ADst[0] := '&';
  ADst[1] := 'q';
  ADst[2] := 'u';
  ADst[3] := 'o';
  ADst[4] := 't';
  ADst[5] := ';';
  Result := ADst + 6;
end;

class function TdxXmlEncodedRawTextWriter.TabEntity(ADst: PChar): PChar;
begin
  ADst[0] := '&';
  ADst[1] := '#';
  ADst[2] := 'x';
  ADst[3] := '9';
  ADst[4] := ';';
  Result := ADst + 5;
end;

class function TdxXmlEncodedRawTextWriter.LineFeedEntity(ADst: PChar): PChar;
begin
  ADst[0] := '&';
  ADst[1] := '#';
  ADst[2] := 'x';
  ADst[3] := 'A';
  ADst[4] := ';';
  Result := ADst + 5;
end;

class function TdxXmlEncodedRawTextWriter.CarriageReturnEntity(ADst: PChar): PChar;
begin
  ADst[0] := '&';
  ADst[1] := '#';
  ADst[2] := 'x';
  ADst[3] := 'D';
  ADst[4] := ';';
  Result := ADst + 5;
end;

class function TdxXmlEncodedRawTextWriter.CharEntity(ADst: PChar; ACh: Char): PChar;
begin
  ADst[0] := '&';
  ADst[1] := '#';
  ADst[2] := 'x';
  Inc(ADst, 3);
  ADst := TdxStringHelper.ToHex(ACh, ADst);
  ADst[0] := ';';
  Inc(ADst);
  Result := ADst;
end;

class function TdxXmlEncodedRawTextWriter.UCS2Entity(ADst: PChar; ACh: Char): PChar;
begin
  ADst[0] := '_';
  ADst[1] := 'x';
  Inc(ADst, 2);
  ADst := TdxStringHelper.ToHex(ACh, ADst);
  ADst[0] := '_';
  Inc(ADst);
  Result := ADst;
end;

class function TdxXmlEncodedRawTextWriter.RawStartCData(ADst: PChar): PChar;
begin
  ADst[0] := '<';
  ADst[1] := '!';
  ADst[2] := '[';
  ADst[3] := 'C';
  ADst[4] := 'D';
  ADst[5] := 'A';
  ADst[6] := 'T';
  ADst[7] := 'A';
  ADst[8] := '[';
  Result := ADst + 9;
end;

class function TdxXmlEncodedRawTextWriter.RawEndCData(ADst: PChar): PChar;
begin
  ADst[0] := ']';
  ADst[1] := ']';
  ADst[2] := '>';
  Result := ADst + 3;
end;

procedure TdxXmlEncodedRawTextWriter.ValidateContentChars(const AChars: string; const APropertyName: string;
  AAllowOnlyWhitespace: Boolean);
var
  I: Integer;
begin
  if AAllowOnlyWhitespace then
  begin
    if not TdxXmlCharType.IsOnlyWhitespace(AChars) then
      raise EdxXmlArgumentException.CreateFmt(SXmlIndentCharsNotWhitespace, [APropertyName]);
  end
  else
  begin
    I := 1;
    while I <= Length(AChars) do
    begin
      if not TdxXmlCharType.IsTextChar(AChars[I]) then
      begin
        case AChars[I] of
          #13, #10, #9:;
          '<', '&', ']':
            raise EdxXmlArgumentException.CreateFmt(SXmlInvalidCharsInIndent, [APropertyName,
              Format(SXmlInvalidCharacter, [AChars[I], CharToHex(AChars[I])])]);
          else
          begin
            if TdxXmlCharType.IsHighSurrogate(AChars[I]) then
            begin
              if I + 1 <= Length(AChars) then
              begin
                if TdxXmlCharType.IsLowSurrogate(AChars[I + 1]) then
                begin
                  Inc(I, 2);
                  Continue;
                end;
              end;
              raise EdxXmlArgumentException.CreateFmt(SXmlInvalidCharsInIndent, [APropertyName, SXmlInvalidSurrogateMissingLowChar]);
            end
            else
              if TdxXmlCharType.IsLowSurrogate(AChars[I]) then
                raise EdxXmlArgumentException.CreateFmt(SXmlInvalidCharsInIndent, [APropertyName,
                  Format(SXmlInvalidHighSurrogateChar, [CharToHex(AChars[I])])]);
          end;
        end;
      end;
      Inc(I);
    end;
  end;
end;

{$ENDREGION}

{ TdxXmlStringWriter }

constructor TdxXmlStringWriter.CreateEx(ASb: TStringBuilder; ASettings: TdxXmlWriterSettings);
begin
  FSb := ASb;
  inherited CreateEx(nil, ASettings);
end;

procedure TdxXmlStringWriter.Write(const ABuffer: string);
begin
  FSb.Append(ABuffer);
end;

procedure TdxXmlStringWriter.Write(const ABuffer: TCharArray; AIndex, ALength: Integer);
begin
  FSb.Append(ABuffer, AIndex, ALength);
end;

{ TdxXmlWellFormedWriter.TElementScope }

procedure TdxXmlWellFormedWriter.TElementScope.&Set(const APrefix, ALocalName, ANamespaceUri: string; APrevNSTop: Integer);
begin
  FPrevNSTop := APrevNSTop;
  FPrefix := APrefix;
  FNamespaceUri := ANamespaceUri;
  FLocalName := ALocalName;
  FXmlSpace := TdxXmlSpace(-1);
  FXmlLang := '';
end;

procedure TdxXmlWellFormedWriter.TElementScope.WriteEndElement(ARawWriter: TdxXmlRawWriter);
begin
  ARawWriter.WriteEndElement(FPrefix, FLocalName, FNamespaceUri);
end;

procedure TdxXmlWellFormedWriter.TElementScope.WriteFullEndElement(ARawWriter: TdxXmlRawWriter);
begin
  ARawWriter.WriteFullEndElement(FPrefix, FLocalName, FNamespaceUri);
end;

{ TdxXmlWellFormedWriter.TNamespace }

procedure TdxXmlWellFormedWriter.TNamespace.&Set(const APrefix, ANamespaceUri: string; AKind: TNamespaceKind);
begin
  FPrefix := APrefix;
  FNamespaceUri := ANamespaceUri;
  FKind := AKind;
  FPrevNsIndex := -1;
end;

procedure TdxXmlWellFormedWriter.TNamespace.WriteDecl(AWriter: TdxXmlWriter; ARawWriter: TdxXmlRawWriter);
begin
  Assert(FKind = TNamespaceKind.NeedToWrite);
  if ARawWriter <> nil then
    ARawWriter.WriteNamespaceDeclaration(FPrefix, FNamespaceUri)
  else
  begin
    if FPrefix = '' then
      AWriter.WriteStartAttribute('', 'xmlns', TdxXmlReservedNs.NsXmlNs)
    else
      AWriter.WriteStartAttribute('xmlns', FPrefix, TdxXmlReservedNs.NsXmlNs);
    AWriter.WriteString(FNamespaceUri);
    AWriter.WriteEndAttribute;
  end;
end;

{ TdxXmlWellFormedWriter.TAttrName }

procedure TdxXmlWellFormedWriter.TAttrName.&Set(const APrefix, ALocalName, ANamespaceUri: string);
begin
  FPrefix := APrefix;
  FNamespaceUri := ANamespaceUri;
  FLocalName := ALocalName;
  FPrev := 0;
end;

function TdxXmlWellFormedWriter.TAttrName.IsDuplicate(const APrefix, ALocalName, ANamespaceUri: string): Boolean;
begin
  Result := (FLocalName = ALocalName) and ((FPrefix = APrefix) or (FNamespaceUri = ANamespaceUri));
end;

{ TdxXmlWellFormedWriter.TAttributeValueCache.TItem }

procedure TdxXmlWellFormedWriter.TAttributeValueCache.TItem.&Set(AType: TItemType; const AData: TValue);
begin
  FType := AType;
  FData := AData;
end;

{ TdxXmlWellFormedWriter.TAttributeValueCache.TdxBufferChunk }

constructor TdxXmlWellFormedWriter.TAttributeValueCache.TBufferChunk.Create(const ABuffer: TCharArray;
  AIndex: Integer; ACount: Integer);
begin
  Buffer := ABuffer;
  Index := AIndex;
  Count := ACount;
end;

{ TAttributeValueCache }

constructor TdxXmlWellFormedWriter.TAttributeValueCache.Create;
begin
  FStringValue := TStringBuilder.Create;
  FLastItem := -1;
end;

destructor TdxXmlWellFormedWriter.TAttributeValueCache.Destroy;
begin
  FStringValue.Free;
  inherited Destroy;
end;

function TdxXmlWellFormedWriter.TAttributeValueCache.GetStringValue: string;
begin
  if FSingleStringValue <> '' then
    Result := FSingleStringValue
  else
    Result := FStringValue.ToString;
end;

procedure TdxXmlWellFormedWriter.TAttributeValueCache.WriteEntityRef(const AName: string);
begin
  if FSingleStringValue <> '' then
    StartComplexValue;

  if AName = 'lt' then
    FStringValue.Append('<')
  else if AName = 'gt' then
    FStringValue.Append('>')
  else if AName = 'quot' then
    FStringValue.Append('"')
  else if AName = 'apos' then
    FStringValue.Append(#$27)
  else if AName = 'amp' then
    FStringValue.Append('&')
  else
  begin
    FStringValue.Append('&');
    FStringValue.Append(AName);
    FStringValue.Append(';');
  end;

  AddItem(TItemType.EntityRef, AName);
end;

procedure TdxXmlWellFormedWriter.TAttributeValueCache.WriteCharEntity(ACh: Char);
begin
  if FSingleStringValue <> '' then
    StartComplexValue;
  FStringValue.Append(ACh);
  AddItem(TItemType.CharEntity, TValue.From<Char>(ACh));
end;

procedure TdxXmlWellFormedWriter.TAttributeValueCache.WriteSurrogateCharEntity(ALowChar: Char; AHighChar: Char);
begin
  if FSingleStringValue <> '' then
    StartComplexValue;
  FStringValue.Append(AHighChar);
  FStringValue.Append(ALowChar);
  AddItem(TItemType.SurrogateCharEntity, ALowChar + AHighChar);
end;

procedure TdxXmlWellFormedWriter.TAttributeValueCache.WriteWhitespace(const AWs: string);
begin
  if FSingleStringValue <> '' then
    StartComplexValue;
  FStringValue.Append(AWs);
  AddItem(TItemType.Whitespace, AWs);
end;

procedure TdxXmlWellFormedWriter.TAttributeValueCache.WriteString(const AText: string);
begin
  if FSingleStringValue <> '' then
    StartComplexValue
  else
  begin
    if FLastItem = -1 then
    begin
      FSingleStringValue := AText;
      Exit;
    end;
  end;

  FStringValue.Append(AText);
  AddItem(TItemType.String, AText);
end;

procedure TdxXmlWellFormedWriter.TAttributeValueCache.WriteChars(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer);
begin
  if FSingleStringValue <> '' then
    StartComplexValue;
  FStringValue.Append(ABuffer, AIndex, ACount);
  AddItem(TItemType.StringChars, TValue.From<TBufferChunk>(TBufferChunk.Create(ABuffer, AIndex, ACount)));
end;

procedure TdxXmlWellFormedWriter.TAttributeValueCache.WriteRaw(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer);
begin
  if FSingleStringValue <> '' then
    StartComplexValue;
  FStringValue.Append(ABuffer, AIndex, ACount);
  AddItem(TItemType.RawChars, TValue.From<TBufferChunk>(TBufferChunk.Create(ABuffer, AIndex, ACount)));
end;

procedure TdxXmlWellFormedWriter.TAttributeValueCache.WriteRaw(const AData: string);
begin
  if FSingleStringValue <> '' then
    StartComplexValue;
  FStringValue.Append(AData);
  AddItem(TItemType.Raw, AData);
end;

procedure TdxXmlWellFormedWriter.TAttributeValueCache.WriteValue(const AValue: string);
begin
  if FSingleStringValue <> '' then
    StartComplexValue;
  FStringValue.Append(AValue);
  AddItem(TItemType.ValueString, AValue);
end;

procedure TdxXmlWellFormedWriter.TAttributeValueCache.Replay(AWriter: TdxXmlWriter);
var
  ABufChunk: TBufferChunk;
  I: Integer;
  AItem: TItem;
  AChars: TCharArray;
begin
  if FSingleStringValue <> '' then
  begin
    AWriter.WriteString(FSingleStringValue);
    Exit;
  end;

  for I := FFirstItem to FLastItem do
  begin
    AItem := FItems[I];
    case AItem.&Type of
      TItemType.EntityRef:
        AWriter.WriteEntityRef(AItem.Data.AsString);
      TItemType.CharEntity:
        AWriter.WriteCharEntity(AItem.Data.AsType<Char>);
      TItemType.SurrogateCharEntity:
        begin
          AChars := AItem.Data.AsType<TCharArray>;
          AWriter.WriteSurrogateCharEntity(AChars[0], AChars[1]);
        end;
      TItemType.Whitespace:
        AWriter.WriteWhitespace(AItem.Data.AsString);
      TItemType.String:
        AWriter.WriteString(AItem.Data.AsString);
      TItemType.StringChars:
        begin
          ABufChunk := AItem.Data.AsType<TBufferChunk>;
          AWriter.WriteChars(ABufChunk.Buffer, ABufChunk.Index, ABufChunk.Count);
        end;
      TItemType.Raw:
        AWriter.WriteRaw(AItem.Data.AsString);
      TItemType.RawChars:
        begin
          ABufChunk := AItem.Data.AsType<TBufferChunk>;
          AWriter.WriteChars(ABufChunk.Buffer, ABufChunk.Index, ABufChunk.Count);
        end;
      TItemType.ValueString:
        AWriter.WriteValue(AItem.Data.AsString);
      else
        Assert(False, 'Unexpected ItemType value.');
    end;
  end;
end;

procedure TdxXmlWellFormedWriter.TAttributeValueCache.Trim;
var
  AValBefore, AValAfter: string;
  I, AEndIndex: Integer;
  AItem: TItem;
  ABufChunk: TBufferChunk;
begin
  if FSingleStringValue <> '' then
  begin
    FSingleStringValue := SysUtils.Trim(FSingleStringValue);
    Exit;
  end;

  AValBefore := FStringValue.ToString;
  AValAfter := SysUtils.Trim(FSingleStringValue);
  if AValBefore <> AValAfter then
  begin
    FStringValue.Free;
    FStringValue := TStringBuilder.Create(AValAfter);
  end;

  I := FFirstItem;
  while (I = FFirstItem) and (I <= FLastItem) do
  begin
    AItem := FItems[I];
    case AItem.&Type of
      TItemType.Whitespace:
        Inc(FFirstItem);
      TItemType.String,
      TItemType.Raw,
      TItemType.ValueString:
        begin
          AItem.Data := SysUtils.TrimLeft(AItem.Data.AsString);
          if AItem.Data.AsString = '' then
            Inc(FFirstItem);
        end;
      TItemType.StringChars,
      TItemType.RawChars:
        begin
          ABufChunk := AItem.Data.AsType<TBufferChunk>;
          AEndIndex := ABufChunk.Index + ABufChunk.Count;
          while (ABufChunk.Index < AEndIndex) and TdxXmlCharType.IsWhiteSpace(ABufChunk.Buffer[ABufChunk.Index]) do
          begin
            Inc(ABufChunk.Index);
            Dec(ABufChunk.Count);
          end;
          AItem.Data := TValue.From<TBufferChunk>(ABufChunk);
          if ABufChunk.Index = AEndIndex then
            Inc(FFirstItem);
        end;
    end;
    Inc(I);
  end;

  I := FLastItem;
  while (I = FLastItem) and (I >= FFirstItem) do
  begin
    AItem := FItems[I];
    case AItem.&Type of
      TItemType.Whitespace:
        Dec(FLastItem);
      TItemType.String,
      TItemType.Raw,
      TItemType.ValueString:
        begin
          AItem.Data := SysUtils.TrimRight(AItem.Data.AsString);
          if AItem.Data.AsString = '' then
            Dec(FLastItem);
        end;
      TItemType.StringChars,
      TItemType.RawChars:
        begin
          ABufChunk := AItem.Data.AsType<TBufferChunk>;
          while (ABufChunk.Count > 0) and TdxXmlCharType.IsWhiteSpace(ABufChunk.Buffer[ABufChunk.Index + ABufChunk.Count - 1]) do
            Dec(ABufChunk.Count);
          AItem.Data := TValue.From<TBufferChunk>(ABufChunk);
          if ABufChunk.Count = 0 then
            Dec(FLastItem);
        end;
    end;
    Dec(I);
  end;
end;

procedure TdxXmlWellFormedWriter.TAttributeValueCache.Clear;
begin
  FSingleStringValue := '';
  FLastItem := -1;
  FFirstItem := 0;
  FStringValue.Length := 0;
end;

procedure TdxXmlWellFormedWriter.TAttributeValueCache.StartComplexValue;
begin
  Assert(FSingleStringValue <> '');
  Assert(FLastItem = -1);

  FStringValue.Append(FSingleStringValue);
  AddItem(TItemType.String, FSingleStringValue);

  FSingleStringValue := '';
end;

procedure TdxXmlWellFormedWriter.TAttributeValueCache.AddItem(AType: TItemType; AData: TValue);
var
  ANewItemIndex: Integer;
begin
  ANewItemIndex := FLastItem + 1;
  if FItems = nil then
    SetLength(FItems, 4)
  else
    if Length(FItems) = ANewItemIndex then
      SetLength(FItems, ANewItemIndex * 2);

  if FItems[ANewItemIndex] = nil then
    FItems[ANewItemIndex] := TItem.Create;

  FItems[ANewItemIndex].&Set(AType, AData);
  FLastItem := ANewItemIndex;
end;

{ TdxXmlWellFormedWriter.TNamespaceResolverProxy }

constructor TdxXmlWellFormedWriter.TNamespaceResolverProxy.Create(AWfWriter: TdxXmlWellFormedWriter);
begin
  FWfWriter := AWfWriter;
end;

function TdxXmlWellFormedWriter.TNamespaceResolverProxy.GetNamespacesInScope(AScope: TdxXmlNamespaceScope): TdxStringsDictionary;
begin
  raise EdxXmlException.Create(SXmlNotImplemented);
end;

function TdxXmlWellFormedWriter.TNamespaceResolverProxy.LookupNamespace(const APrefix: string): string;
begin
  Result := FWfWriter.LookupNamespace(APrefix);
end;

function TdxXmlWellFormedWriter.TNamespaceResolverProxy.LookupPrefix(const ANamespaceName: string): string;
begin
  Result := FWfWriter.LookupPrefix(ANamespaceName);
end;

{ TdxXmlWellFormedWriter }

constructor TdxXmlWellFormedWriter.Create(AWriter: TdxXmlRawWriter; ASettings: TdxXmlWriterSettings);
var
  ADefaultNs: string;
begin
  FSpecAttr := TSpecialAttribute.No;

  Assert(AWriter <> nil);
  Assert(ASettings <> nil);
  FWriter := AWriter;
  if not Supports(AWriter, IdxXmlNamespaceResolver, FPredefinedNamespaces) then
    FPredefinedNamespaces := nil;
  FWriter.NamespaceResolver := TNamespaceResolverProxy.Create(Self);

  FCheckCharacters := ASettings.CheckCharacters;
  FOmitDuplNamespaces := ASettings.NamespaceHandling = TdxXmlNamespaceHandling.OmitDuplicates;
  FWriteEndDocumentOnClose := ASettings.WriteEndDocumentOnClose;
  FConformanceLevel := ASettings.ConformanceLevel;

  if FConformanceLevel = TdxXmlConformanceLevel.Document then
    FStateTable := FStateTableDocument
  else
    FStateTable := FStateTableAuto;

  FCurrentState := TStates.Start;

  SetLength(FNsStack, NamespaceStackInitialSize);
  FNsStack[0].&Set('xmlns', TdxXmlReservedNs.NsXmlNs, TNamespaceKind.Special);
  FNsStack[1].&Set('xml', TdxXmlReservedNs.NsXml, TNamespaceKind.Special);
  if FPredefinedNamespaces = nil then
    FNsStack[2].&Set('', '', TNamespaceKind.Implied)
  else
  begin
    ADefaultNs := FPredefinedNamespaces.LookupNamespace('');
    FNsStack[2].&Set('', ADefaultNs, TNamespaceKind.Implied);
  end;
  FNsTop := 2;

  SetLength(FElemScopeStack, ElementStackInitialSize);
  FElemScopeStack[0].&Set('', '', '', FNsTop);
  FElemScopeStack[0].XmlSpace := TdxXmlSpace.None;
  FElemScopeStack[0].XmlLang := '';
  FElemTop := 0;

  SetLength(FAttrStack, AttributeArrayInitialSize);

  FHasher := TdxSecureStringHasher.Create;
end;

destructor TdxXmlWellFormedWriter.Destroy;
begin
  if WriteState <> TdxXmlWriteState.Closed then
    Close;
  FWriter.Free;
  FNsHashTable.Free;
  FAttrHashTable.Free;
  FAttrValueCache.Free;
  inherited Destroy;
end;

class constructor TdxXmlWellFormedWriter.Initialize;
begin
  FStateToWriteState := TArray<TdxXmlWriteState>.Create(
    TdxXmlWriteState.Start,
    TdxXmlWriteState.Prolog,
    TdxXmlWriteState.Prolog,
    TdxXmlWriteState.Element,
    TdxXmlWriteState.Content,
    TdxXmlWriteState.Content,
    TdxXmlWriteState.Attribute,
    TdxXmlWriteState.Content,
    TdxXmlWriteState.Attribute,
    TdxXmlWriteState.Attribute,
    TdxXmlWriteState.Content,
    TdxXmlWriteState.Attribute,
    TdxXmlWriteState.Attribute,
    TdxXmlWriteState.Attribute,
    TdxXmlWriteState.Attribute,
    TdxXmlWriteState.Closed,
    TdxXmlWriteState.Error
  );
  FStateTableDocument := TArray<TState>.Create(
    { Token.StartDocument  } TStates.Document,       TStates.Error,     TStates.Error,       TStates.Error,           TStates.Error,      TStates.PostB64Cont,    TStates.Error,         TStates.Error,          TStates.Error,          TStates.Error,         TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error,
    { Token.EndDocument    } TStates.Error,          TStates.Error,     TStates.Error,       TStates.Error,           TStates.Error,      TStates.PostB64Cont,    TStates.Error,         TStates.EndDocument,    TStates.Error,          TStates.Error,         TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error,
    { Token.PI             } TStates.StartDoc,       TStates.TopLevel,  TStates.Document,    TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.AfterRootEle,   TStates.EndAttrSCont,   TStates.EndAttrSCont,  TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error,
    { Token.Comment        } TStates.StartDoc,       TStates.TopLevel,  TStates.Document,    TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.AfterRootEle,   TStates.EndAttrSCont,   TStates.EndAttrSCont,  TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error,
    { Token.Dtd            } TStates.StartDoc,       TStates.TopLevel,  TStates.Document,    TStates.Error,           TStates.Error,      TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Error,          TStates.Error,          TStates.Error,         TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error,
    { Token.StartElement   } TStates.StartDocEle,    TStates.Element,   TStates.Element,     TStates.StartContentEle, TStates.Element,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Error,          TStates.EndAttrSEle,    TStates.EndAttrSEle,   TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error,
    { Token.EndElement     } TStates.Error,          TStates.Error,     TStates.Error,       TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Error,          TStates.EndAttrEEle,    TStates.EndAttrEEle,   TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error,
    { Token.StartAttribute } TStates.Error,          TStates.Error,     TStates.Error,       TStates.Attribute,       TStates.Error,      TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Error,          TStates.EndAttrSAttr,   TStates.EndAttrSAttr,  TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error,
    { Token.EndAttribute   } TStates.Error,          TStates.Error,     TStates.Error,       TStates.Error,           TStates.Error,      TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Error,          TStates.Element,        TStates.Element,       TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error,
    { Token.Text           } TStates.Error,          TStates.Error,     TStates.Error,       TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Error,          TStates.Attribute,      TStates.SpecialAttr,   TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error,
    { Token.CData          } TStates.Error,          TStates.Error,     TStates.Error,       TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Error,          TStates.EndAttrSCont,   TStates.EndAttrSCont,  TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error,
    { Token.AtomicValue    } TStates.Error,          TStates.Error,     TStates.Error,       TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Error,          TStates.Attribute,      TStates.Error,         TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error,
    { Token.Base64         } TStates.Error,          TStates.Error,     TStates.Error,       TStates.StartContentB64, TStates.B64Content, TStates.B64Content,     TStates.B64Attribute,  TStates.Error,          TStates.B64Attribute,   TStates.Error,         TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error,
    { Token.RawData        } TStates.StartDoc,       TStates.Error,     TStates.Document,    TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.AfterRootEle,   TStates.Attribute,      TStates.SpecialAttr,   TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error,
    { Token.Whitespace     } TStates.StartDoc,       TStates.TopLevel,  TStates.Document,    TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.AfterRootEle,   TStates.Attribute,      TStates.SpecialAttr,   TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error
  );
  FStateTableAuto := TArray<TState>.Create(
    { Token.StartDocument  } TStates.Document,       TStates.Error,         TStates.Error,       TStates.Error,           TStates.Error,      TStates.PostB64Cont,    TStates.Error,         TStates.Error,          TStates.Error,          TStates.Error,         TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error, { Token.StartDocument  }
    { Token.EndDocument    } TStates.Error,          TStates.Error,         TStates.Error,       TStates.Error,           TStates.Error,      TStates.PostB64Cont,    TStates.Error,         TStates.EndDocument,    TStates.Error,          TStates.Error,         TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error, { Token.EndDocument    }
    { Token.PI             } TStates.TopLevel,       TStates.TopLevel,      TStates.Error,       TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.AfterRootEle,   TStates.EndAttrSCont,   TStates.EndAttrSCont,  TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error, { Token.PI             }
    { Token.Comment        } TStates.TopLevel,       TStates.TopLevel,      TStates.Error,       TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.AfterRootEle,   TStates.EndAttrSCont,   TStates.EndAttrSCont,  TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error, { Token.Comment        }
    { Token.Dtd            } TStates.StartDoc,       TStates.TopLevel,      TStates.Error,       TStates.Error,           TStates.Error,      TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Error,          TStates.Error,          TStates.Error,         TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error, { Token.Dtd            }
    { Token.StartElement   } TStates.StartFragEle,   TStates.Element,       TStates.Error,       TStates.StartContentEle, TStates.Element,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Element,        TStates.EndAttrSEle,    TStates.EndAttrSEle,   TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error, { Token.StartElement   }
    { Token.EndElement     } TStates.Error,          TStates.Error,         TStates.Error,       TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Error,          TStates.EndAttrEEle,    TStates.EndAttrEEle,   TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error, { Token.EndElement     }
    { Token.StartAttribute } TStates.RootLevelAttr,  TStates.Error,         TStates.Error,       TStates.Attribute,       TStates.Error,      TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Error,          TStates.EndAttrSAttr,   TStates.EndAttrSAttr,  TStates.Error,        TStates.StartRootLevelAttr, TStates.StartRootLevelAttr, TStates.PostB64RootAttr,   TStates.RootLevelAttr,      TStates.Error, { Token.StartAttribute }
    { Token.EndAttribute   } TStates.Error,          TStates.Error,         TStates.Error,       TStates.Error,           TStates.Error,      TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Error,          TStates.Element,        TStates.Element,       TStates.Error,        TStates.AfterRootLevelAttr, TStates.AfterRootLevelAttr, TStates.PostB64RootAttr,   TStates.Error,              TStates.Error, { Token.EndAttribute   }
    { Token.Text           } TStates.StartFragCont,  TStates.StartFragCont, TStates.Error,       TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Content,        TStates.Attribute,      TStates.SpecialAttr,   TStates.Error,        TStates.RootLevelAttr,      TStates.RootLevelSpecAttr,  TStates.PostB64RootAttr,   TStates.Error,              TStates.Error, { Token.Text           }
    { Token.CData          } TStates.StartFragCont,  TStates.StartFragCont, TStates.Error,       TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Content,        TStates.EndAttrSCont,   TStates.EndAttrSCont,  TStates.Error,        TStates.Error,              TStates.Error,              TStates.Error,             TStates.Error,              TStates.Error, { Token.CData          }
    { Token.AtomicValue    } TStates.StartFragCont,  TStates.StartFragCont, TStates.Error,       TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Content,        TStates.Attribute,      TStates.Error,         TStates.Error,        TStates.RootLevelAttr,      TStates.Error,              TStates.PostB64RootAttr,   TStates.Error,              TStates.Error, { Token.AtomicValue    }
    { Token.Base64         } TStates.StartFragB64,   TStates.StartFragB64,  TStates.Error,       TStates.StartContentB64, TStates.B64Content, TStates.B64Content,     TStates.B64Attribute,  TStates.B64Content,     TStates.B64Attribute,   TStates.Error,         TStates.Error,        TStates.RootLevelB64Attr,   TStates.Error,              TStates.RootLevelB64Attr,  TStates.Error,              TStates.Error, { Token.Base64         }
    { Token.RawData        } TStates.StartFragCont,  TStates.TopLevel,      TStates.Error,       TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.Content,        TStates.Attribute,      TStates.SpecialAttr,   TStates.Error,        TStates.RootLevelAttr,      TStates.RootLevelSpecAttr,  TStates.PostB64RootAttr,   TStates.AfterRootLevelAttr, TStates.Error, { Token.RawData        }
    { Token.Whitespace     } TStates.TopLevel,       TStates.TopLevel,      TStates.Error,       TStates.StartContent,    TStates.Content,    TStates.PostB64Cont,    TStates.PostB64Attr,   TStates.AfterRootEle,   TStates.Attribute,      TStates.SpecialAttr,   TStates.Error,        TStates.RootLevelAttr,      TStates.RootLevelSpecAttr,  TStates.PostB64RootAttr,   TStates.AfterRootLevelAttr, TStates.Error  { Token.Whitespace     }
  );
end;

function TdxXmlWellFormedWriter.GetWriteState: TdxXmlWriteState;
begin
  if FCurrentState <= TStates.Error then
    Result := FStateToWriteState[FCurrentState]
  else
  begin
    Assert(False, 'Expected currentState <= State.Error ');
    Result := TdxXmlWriteState.Error;
  end;
end;

function TdxXmlWellFormedWriter.GetSettings: TdxXmlWriterSettings;
begin
  Result := FWriter.Settings;
  Result.ConformanceLevel := FConformanceLevel;
  if FOmitDuplNamespaces then
    Result.NamespaceHandling := TdxXmlNamespaceHandling.OmitDuplicates;
end;

procedure TdxXmlWellFormedWriter.WriteStartDocument;
begin
  WriteStartDocumentImpl(TdxXmlStandalone.Omit);
end;

procedure TdxXmlWellFormedWriter.WriteStartDocument(AStandalone: Boolean);
begin
  if AStandalone then
    WriteStartDocumentImpl(TdxXmlStandalone.Yes)
  else
    WriteStartDocumentImpl(TdxXmlStandalone.No);
end;

procedure TdxXmlWellFormedWriter.WriteEndDocument;
var
  APrevState: TState;
begin
  try
    while FElemTop > 0 do
      WriteEndElement;

    APrevState := FCurrentState;
    AdvanceState(TToken.EndDocument);

    if APrevState <> TStates.AfterRootEle then
      raise EdxXmlArgumentException.Create(SXmlNoRoot);

    FWriter.WriteEndDocument;
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteStartElement(APrefix: string; const ALocalName: string; ANs: string);
var
  ATop: Integer;
begin
  try
    if ALocalName = '' then
      raise EdxXmlArgumentException.Create(SXmlEmptyLocalName);

    CheckNCName(ALocalName);
    AdvanceState(TToken.StartElement);

    if APrefix = '' then
    begin
      if ANs <> '' then
        APrefix := LookupPrefix(ANs);
    end
    else
    begin
      CheckNCName(APrefix);
      if ANs = '' then
        ANs := LookupNamespace(APrefix);

      if ANs = '' then
        raise EdxXmlArgumentException.Create(SXmlPrefixForEmptyNs);
    end;

    if ANs = '' then
    begin
      ANs := LookupNamespace(APrefix);
      if ANs = '' then
      begin
        Assert(Length(APrefix) = 0);
        ANs := '';
      end;
    end;

    if FElemTop = 0 then
      FWriter.OnRootElement(FConformanceLevel);

    FWriter.WriteStartElement(APrefix, ALocalName, ANs);

    Inc(FElemTop);
    ATop := FElemTop;
    if ATop = Length(FElemScopeStack) then
      SetLength(FElemScopeStack, ATop * 2);

    FElemScopeStack[ATop].&Set(APrefix, ALocalName, ANs, FNsTop);

    PushNamespaceImplicit(APrefix, ANs);

    if FAttrCount >= MaxAttrDuplWalkCount then
      FAttrHashTable.Clear;
    FAttrCount := 0;
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteEndElement;
var
  ATop, APrevNsTop: Integer;
begin
  try
    AdvanceState(TToken.EndElement);

    ATop := FElemTop;
    if ATop = 0 then
      raise EdxXmlArgumentException.Create(SXmlNoStartTag);

    FElemScopeStack[ATop].WriteEndElement(FWriter);

    APrevNsTop := FElemScopeStack[ATop].PrevNSTop;
    if FUseNsHashTable and (APrevNsTop < FNsTop) then
      PopNamespaces(APrevNsTop + 1, FNsTop);

    FNsTop := APrevNsTop;
    Dec(ATop);
    FElemTop := ATop;

    if ATop = 0 then
    begin
      if FConformanceLevel = TdxXmlConformanceLevel.Document then
        FCurrentState := TStates.AfterRootEle
      else
        FCurrentState := TStates.TopLevel;
    end;
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteFullEndElement;
var
  ATop, APrevNsTop: Integer;
begin
  try
    AdvanceState(TToken.EndElement);

    ATop := FElemTop;
    if ATop = 0 then
      raise EdxXmlException.Create(SXmlNoStartTag);

    FElemScopeStack[ATop].WriteFullEndElement(FWriter);

    APrevNsTop := FElemScopeStack[ATop].PrevNSTop;
    if FUseNsHashTable and (APrevNsTop < FNsTop) then
      PopNamespaces(APrevNsTop + 1, FNsTop);

    FNsTop := APrevNsTop;
    Dec(ATop);
    FElemTop := ATop;

    if ATop = 0 then
    begin
      if FConformanceLevel = TdxXmlConformanceLevel.Document then
        FCurrentState := TStates.AfterRootEle
      else
        FCurrentState := TStates.TopLevel;
    end;
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteStartAttribute(APrefix: string; ALocalName: string; ANamespaceName: string);
label
  SkipPushAndWrite;
var
  ADefinedNs: string;
begin
  try
    if ALocalName = '' then
    begin
      if APrefix = 'xmlns' then
      begin
        ALocalName := 'xmlns';
        APrefix := '';
      end
      else
        raise EdxXmlArgumentException.Create(SXmlEmptyLocalName);
    end;
    CheckNCName(ALocalName);

    AdvanceState(TToken.StartAttribute);

    if APrefix = '' then
    begin
      if ANamespaceName <> '' then
      begin
        if not ((ALocalName = 'xmlns') and (ANamespaceName = TdxXmlReservedNs.NsXmlNs)) then
          APrefix := LookupPrefix(ANamespaceName);
      end;
    end;
    if ANamespaceName = '' then
    begin
      if APrefix <> '' then
        ANamespaceName := LookupNamespace(APrefix);
    end;

    if APrefix = '' then
    begin
      if (ALocalName[1] = 'x') and (ALocalName = 'xmlns') then
      begin
        if (ANamespaceName <> '') and (ANamespaceName <> TdxXmlReservedNs.NsXmlNs) then
          raise EdxXmlArgumentException.Create(SXmlXmlnsPrefix);

        FCurDeclPrefix := '';
        SetSpecialAttribute(TSpecialAttribute.DefaultXmlns);
        goto SkipPushAndWrite;
      end
      else
        if ANamespaceName <> '' then
        begin
          APrefix := LookupPrefix(ANamespaceName);
          if APrefix = '' then
            APrefix := GeneratePrefix;
        end;
    end
    else
    begin
      if APrefix[1] = 'x' then
      begin
        if APrefix = 'xmlns' then
        begin
          if (ANamespaceName <> '') and (ANamespaceName <> TdxXmlReservedNs.NsXmlNs) then
            raise EdxXmlArgumentException.Create(SXmlXmlnsPrefix);

          FCurDeclPrefix := ALocalName;
          SetSpecialAttribute(TSpecialAttribute.PrefixedXmlns);
          goto SkipPushAndWrite;
        end
        else
          if APrefix = 'xml' then
          begin
            if (ANamespaceName <> '') and (ANamespaceName <> TdxXmlReservedNs.NsXml) then
              raise EdxXmlArgumentException.Create('Xml_XmlPrefix');

            if ALocalName = 'space' then
            begin
              SetSpecialAttribute(TSpecialAttribute.XmlSpace);
              goto SkipPushAndWrite;
            end;
            if ALocalName = 'lang' then
            begin
              SetSpecialAttribute(TSpecialAttribute.XmlLang);
              goto SkipPushAndWrite;
            end;
          end;
      end;

      CheckNCName(APrefix);

      if ANamespaceName = '' then
        APrefix := ''
      else
      begin
        ADefinedNs := LookupLocalNamespace(APrefix);
        if (ADefinedNs <> '') and (ADefinedNs <> ANamespaceName) then
          APrefix := GeneratePrefix;
      end;
    end;

    if APrefix <> '' then
      PushNamespaceImplicit(APrefix, ANamespaceName);

SkipPushAndWrite:
    AddAttribute(APrefix, ALocalName, ANamespaceName);

    if FSpecAttr = TSpecialAttribute.No then
      FWriter.WriteStartAttribute(APrefix, ALocalName, ANamespaceName);
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteEndAttribute;
var
  AValue: string;
begin
  try
    AdvanceState(TToken.EndAttribute);

    if FSpecAttr <> TSpecialAttribute.No then
    begin
      case FSpecAttr of
        TSpecialAttribute.DefaultXmlns:
          begin
            AValue := FAttrValueCache.StringValue;
            if PushNamespaceExplicit('', AValue) then
            begin
              if FWriter.SupportsNamespaceDeclarationInChunks then
              begin
                FWriter.WriteStartNamespaceDeclaration('');
                FAttrValueCache.Replay(FWriter);
                FWriter.WriteEndNamespaceDeclaration;
              end
              else
                FWriter.WriteNamespaceDeclaration('', AValue);
            end;
            FCurDeclPrefix := '';
          end;
        TSpecialAttribute.PrefixedXmlns:
          begin
            AValue := FAttrValueCache.StringValue;
            if AValue = '' then
              raise EdxXmlArgumentException.Create(SXmlPrefixForEmptyNs);

            if (AValue = TdxXmlReservedNs.NsXmlNs) or ((AValue = TdxXmlReservedNs.NsXml) and (FCurDeclPrefix <> 'xml')) then
              raise EdxXmlArgumentException.Create(SXmlCanNotBindToReservedNamespace);

            if PushNamespaceExplicit(FCurDeclPrefix, AValue) then
            begin
              if FWriter.SupportsNamespaceDeclarationInChunks then
              begin
                FWriter.WriteStartNamespaceDeclaration(FCurDeclPrefix);
                FAttrValueCache.Replay(FWriter);
                FWriter.WriteEndNamespaceDeclaration;
              end
              else
                FWriter.WriteNamespaceDeclaration(FCurDeclPrefix, AValue);
            end;
            FCurDeclPrefix := '';
          end;
        TSpecialAttribute.XmlSpace:
          begin
            FAttrValueCache.Trim;
            AValue := FAttrValueCache.StringValue;

            if AValue = 'default' then
              FElemScopeStack[FElemTop].xmlSpace := TdxXmlSpace.Default
            else
              if AValue = 'preserve' then
                FElemScopeStack[FElemTop].xmlSpace := TdxXmlSpace.Preserve
              else
                raise EdxXmlArgumentException.CreateFmt(SXmlInvalidXmlSpace, [AValue]);

            FWriter.WriteStartAttribute('xml', 'space', TdxXmlReservedNs.NsXml);
            FAttrValueCache.Replay(FWriter);
            FWriter.WriteEndAttribute;
          end;
        TSpecialAttribute.XmlLang:
          begin
            AValue := FAttrValueCache.StringValue;
            FElemScopeStack[FElemTop].xmlLang := AValue;
            FWriter.WriteStartAttribute('xml', 'lang', TdxXmlReservedNs.NsXml);
            FAttrValueCache.Replay(FWriter);
            FWriter.WriteEndAttribute;
          end;
      end;
      FSpecAttr := TSpecialAttribute.No;
      FAttrValueCache.Clear;
    end
    else
      FWriter.WriteEndAttribute;
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteCData(const AText: string);
begin
  try
    AdvanceState(TToken.CData);
    FWriter.WriteCData(AText);
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteComment(const AText: string);
begin
  try
    AdvanceState(TToken.Comment);
    FWriter.WriteComment(AText);
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteProcessingInstruction(const AName, AText: string);
begin
  try
    if AName = '' then
      raise EdxXmlArgumentException.Create(SXmlEmptyName);

    CheckNCName(AName);

    if (Length(AName) = 3) and SameText(AName, 'xml') then
    begin
      if FCurrentState <> TStates.Start then
        if FConformanceLevel = TdxXmlConformanceLevel.Document then
          raise EdxXmlArgumentException.Create(SXmlDupXmlDecl)
        else
          raise EdxXmlArgumentException.Create(SXmlCannotWriteXmlDecl);

      FXmlDeclFollows := True;
      AdvanceState(TToken.PI);

      FWriter.WriteXmlDeclaration(AText);
    end
    else
    begin
      AdvanceState(TToken.PI);
      FWriter.WriteProcessingInstruction(AName, AText);
    end;
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteEntityRef(const AName: string);
begin
  try
    if AName = '' then
      raise EdxXmlArgumentException.Create(SXmlEmptyName);

    CheckNCName(AName);

    AdvanceState(TToken.Text);
    if SaveAttrValue then
      FAttrValueCache.WriteEntityRef(AName)
    else
      FWriter.WriteEntityRef(AName);
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteCharEntity(ACh: Char);
begin
  try
  {$IFNDEF DELPHIXE4}
    if TCharacter.IsSurrogate(ACh) then
      raise EdxXmlArgumentException.Create(SXmlInvalidSurrogateMissingLowChar);
  {$ELSE}
    if ACh.IsSurrogate then
      raise EdxXmlArgumentException.Create(SXmlInvalidSurrogateMissingLowChar);
  {$ENDIF}
    AdvanceState(TToken.Text);
    if SaveAttrValue then
      FAttrValueCache.WriteCharEntity(ACh)
    else
      FWriter.WriteCharEntity(ACh);
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteSurrogateCharEntity(ALowChar: Char; AHighChar: Char);
begin
  try
  {$IFNDEF DELPHIXE4}
    if not TCharacter.IsSurrogatePair(AHighChar, ALowChar) then
      raise CreateInvalidSurrogatePairException(ALowChar, AHighChar);
  {$ELSE}
    if not Char.IsSurrogatePair(AHighChar, ALowChar) then
      raise CreateInvalidSurrogatePairException(ALowChar, AHighChar);
  {$ENDIF}

    AdvanceState(TToken.Text);
    if SaveAttrValue then
      FAttrValueCache.WriteSurrogateCharEntity(ALowChar, AHighChar)
    else
      FWriter.WriteSurrogateCharEntity(ALowChar, AHighChar);
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteWhitespace(const AWs: string);
begin
  try
    if not TdxXmlCharType.IsOnlyWhitespace(AWs) then
      raise EdxXmlArgumentException.Create(SXmlNonWhitespace);

    AdvanceState(TToken.Whitespace);
    if SaveAttrValue then
      FAttrValueCache.WriteWhitespace(AWs)
    else
      FWriter.WriteWhitespace(AWs);
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteString(const AText: string);
begin
  try
    if AText = '' then
      Exit;

    AdvanceState(TToken.Text);
    if SaveAttrValue then
      FAttrValueCache.WriteString(AText)
    else
      FWriter.WriteString(AText);
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteChars(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer);
begin
  try
    if ABuffer = nil then
      raise EdxXmlArgumentNullException.Create('buffer');

    if AIndex < 0 then
      raise EdxXmlArgumentOutOfRangeException.Create('index');

    if ACount < 0 then
      raise EdxXmlArgumentOutOfRangeException.Create('count');

    if ACount > Length(ABuffer) - AIndex then
      raise EdxXmlArgumentOutOfRangeException.Create('count');

    AdvanceState(TToken.Text);
    if SaveAttrValue then
      FAttrValueCache.WriteChars(ABuffer, AIndex, ACount)
    else
      FWriter.WriteChars(ABuffer, AIndex, ACount);
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteRaw(const ABuffer: TCharArray; AIndex: Integer; ACount: Integer);
begin
  try
    if ABuffer = nil then
      raise EdxXmlArgumentNullException.Create('buffer');

    if AIndex < 0 then
      raise EdxXmlArgumentOutOfRangeException.Create('index');

    if ACount < 0 then
      raise EdxXmlArgumentOutOfRangeException.Create('count');

    if ACount > Length(ABuffer) - AIndex then
      raise EdxXmlArgumentOutOfRangeException.Create('count');

    AdvanceState(TToken.RawData);
    if SaveAttrValue then
      FAttrValueCache.WriteRaw(ABuffer, AIndex, ACount)
    else
      FWriter.WriteRaw(ABuffer, AIndex, ACount);
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteRaw(const AData: string);
begin
  try
    if AData = '' then
      Exit;

    AdvanceState(TToken.RawData);
    if SaveAttrValue then
      FAttrValueCache.WriteRaw(AData)
    else
      FWriter.WriteRaw(AData);
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteBase64(const ABuffer: TBytes; AIndex: Integer; ACount: Integer);
begin
  try
    if ABuffer = nil then
      raise EdxXmlArgumentNullException.Create('buffer');
    if AIndex < 0 then
      raise EdxXmlArgumentOutOfRangeException.Create('index');
    if ACount < 0 then
      raise EdxXmlArgumentOutOfRangeException.Create('count');
    if ACount > Length(ABuffer) - AIndex then
      raise EdxXmlArgumentOutOfRangeException.Create('count');

    AdvanceState(TToken.Base64);
    FWriter.WriteBase64(ABuffer, AIndex, ACount);
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.Close;
begin
  if FCurrentState <> TStates.Closed then
  begin
    try
      if FWriteEndDocumentOnClose then
      begin
        while (FCurrentState <> TStates.Error) and (FElemTop > 0) do
          WriteEndElement;
      end
      else
      begin
        if (FCurrentState <> TStates.Error) and (FElemTop > 0) then
        try
          AdvanceState(TToken.EndElement);
        except
          FCurrentState := TStates.Error;
          raise;
        end;
      end;

      if InBase64 then
        FWriter.WriteEndBase64;

      FWriter.Flush;
    finally
      try
        FWriter.Close;
      finally
        FCurrentState := TStates.Closed;
      end;
    end;
  end;
end;

procedure TdxXmlWellFormedWriter.Flush;
begin
  try
    FWriter.Flush;
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

function TdxXmlWellFormedWriter.LookupPrefix(const ANs: string): string;
var
  I: Integer;
  APrefix: string;
begin
  try
    if ANs = '' then
      raise EdxXmlArgumentNullException.Create('ns');
    I := FNsTop;
    while I >= 0 do
    begin
      if FNsStack[I].NamespaceUri = ANs then
      begin
        APrefix := FNsStack[I].Prefix;
        Inc(I);
        while I <= FNsTop do
        begin
          if FNsStack[I].Prefix = APrefix then
            Exit('');
          Inc(I);
        end;
        Exit(APrefix);
      end;
      Dec(I);
    end;
    if FPredefinedNamespaces <> nil then
      Result := FPredefinedNamespaces.LookupPrefix(ANs)
    else
      Result := '';
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

function TdxXmlWellFormedWriter.GetXmlSpace: TdxXmlSpace;
var
  I: Integer;
begin
  I := FElemTop;
  while (I >= 0) and (FElemScopeStack[I].XmlSpace = TdxXmlSpace(-1)) do
    Dec(I);
  Assert(I >= 0);
  Result := FElemScopeStack[I].XmlSpace;
end;

function TdxXmlWellFormedWriter.GetXmlLang: string;
var
  I: Integer;
begin
  I := FElemTop;
  while (I > 0) and (FElemScopeStack[I].XmlLang = '') do
    Dec(I);
  Assert(I >= 0);
  Result := FElemScopeStack[I].XmlLang;
end;

procedure TdxXmlWellFormedWriter.ThrowInvalidStateTransition(AToken: TToken; ACurrentState: TState);
var
  AWrongTokenMessage: string;
begin
  AWrongTokenMessage := Format(SXmlWrongToken, [Ord(AToken), Ord(ACurrentState)]);
  case ACurrentState of
    TStates.AfterRootEle, TStates.Start:
      if FConformanceLevel = TdxXmlConformanceLevel.Document then
        raise EdxXmlInvalidOperationException.Create(AWrongTokenMessage + ' ' + SXmlConformanceLevelFragment);
  end;
  raise EdxXmlInvalidOperationException.Create(AWrongTokenMessage);
end;

procedure TdxXmlWellFormedWriter.WriteQualifiedName(const ALocalName, ANs: string);
var
  APrefix: string;
begin
  try
    if ALocalName = '' then
      raise EdxXmlArgumentException.Create(SXmlEmptyLocalName);

    CheckNCName(ALocalName);

    AdvanceState(TToken.Text);
    APrefix := '';
    if ANs <> '' then
    begin
      APrefix := LookupPrefix(ANs);
      if APrefix = '' then
      begin
        if FCurrentState <> TStates.Attribute then
          raise EdxXmlArgumentException.CreateFmt(SXmlUndefNamespace, [ANs]);

        APrefix := GeneratePrefix;
        PushNamespaceImplicit(APrefix, ANs);
      end;
    end;

    if SaveAttrValue then
    begin
      if APrefix <> '' then
      begin
        WriteString(APrefix);
        WriteString(':');
      end;
      WriteString(ALocalName);
    end
    else
      FWriter.WriteQualifiedName(APrefix, ALocalName, ANs);
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteValue(const AValue: string);
begin
  try
    if AValue = '' then
      Exit;
    if SaveAttrValue then
    begin
      AdvanceState(TToken.Text);
      FAttrValueCache.WriteValue(AValue);
    end
    else
    begin
      AdvanceState(TToken.AtomicValue);
      FWriter.WriteValue(AValue);
    end;
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.WriteBinHex(const ABuffer: TBytes; AIndex: Integer; ACount: Integer);
begin
  if IsClosedOrErrorState then
    raise EdxXmlInvalidOperationException.Create(SXmlClosedOrError);
  try
    AdvanceState(TToken.Text);
    inherited WriteBinHex(ABuffer, AIndex, ACount);
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

function TdxXmlWellFormedWriter.GetSaveAttrValue: Boolean;
begin
  Result := FSpecAttr <> TSpecialAttribute.No;
end;

function TdxXmlWellFormedWriter.GetInBase64: Boolean;
begin
  Result := FCurrentState in [TStates.B64Content, TStates.B64Attribute, TStates.RootLevelB64Attr];
end;

procedure TdxXmlWellFormedWriter.SetSpecialAttribute(ASpecial: TSpecialAttribute);
begin
  FSpecAttr := ASpecial;
  if TStates.Attribute = FCurrentState then
    FCurrentState := TStates.SpecialAttr
  else
    if TStates.RootLevelAttr = FCurrentState then
      FCurrentState := TStates.RootLevelSpecAttr
    else
      Assert(False, 'State.Attribute == currentState || State.RootLevelAttr == currentState');

  if FAttrValueCache = nil then
    FAttrValueCache := TAttributeValueCache.Create;
end;

procedure TdxXmlWellFormedWriter.WriteStartDocumentImpl(AStandalone: TdxXmlStandalone);
begin
  try
    AdvanceState(TToken.StartDocument);

    if FConformanceLevel = TdxXmlConformanceLevel.Auto then
    begin
      FConformanceLevel := TdxXmlConformanceLevel.Document;
      FStateTable := FStateTableDocument;
    end
    else
      if FConformanceLevel = TdxXmlConformanceLevel.Fragment then
        raise EdxXmlInvalidOperationException.Create(SXmlCannotStartDocumentOnFragment);

    if not FXmlDeclFollows then
      FWriter.WriteXmlDeclaration(AStandalone);
  except
    FCurrentState := TStates.Error;
    raise;
  end;
end;

procedure TdxXmlWellFormedWriter.StartFragment;
begin
  FConformanceLevel := TdxXmlConformanceLevel.Fragment;
  Assert(FStateTable = FStateTableAuto);
end;

procedure TdxXmlWellFormedWriter.PushNamespaceImplicit(const APrefix, ANs: string);
var
  AKind: TNamespaceKind;
  AExistingNsIndex: Integer;
  ADefinedNs: string;
begin
  AExistingNsIndex := LookupNamespaceIndex(APrefix);
  if AExistingNsIndex <> -1 then
  begin
    if AExistingNsIndex > FElemScopeStack[FElemTop].PrevNSTop then
    begin
      if FNsStack[AExistingNsIndex].NamespaceUri <> ANs then
        raise EdxXmlException.CreateFmt(SXmlRedefinePrefix, [APrefix, FNsStack[AExistingNsIndex].NamespaceUri, ANs]);
      Exit;
    end
    else
    begin
      if FNsStack[AExistingNsIndex].Kind = TNamespaceKind.Special then
      begin
        if APrefix = 'xml' then
        begin
          if ANs <> FNsStack[AExistingNsIndex].namespaceUri then
            raise EdxXmlArgumentException.Create(SXmlXmlPrefix)
          else
            AKind := TNamespaceKind.Implied;
        end
        else
        begin
          Assert(APrefix = 'xmlns');
          raise EdxXmlArgumentException.Create(SXmlXmlnsPrefix);
        end;
      end
      else
      begin
        if (FNsStack[AExistingNsIndex].NamespaceUri = ANs) then
          AKind := TNamespaceKind.Implied
        else
          AKind := TNamespaceKind.NeedToWrite;
      end;
    end;
  end
  else
  begin
    if ((ANs = TdxXmlReservedNs.NsXml) and (APrefix <> 'xml')) or ((ANs = TdxXmlReservedNs.NsXmlNs) and (APrefix <> 'xmlns')) then
      raise EdxXmlArgumentException.CreateFmt(SXmlNamespaceDeclXmlXmlns, [APrefix]);
    if FPredefinedNamespaces <> nil then
    begin
      ADefinedNs := FPredefinedNamespaces.LookupNamespace(APrefix);
      if ADefinedNs = ANs then
        AKind := TNamespaceKind.Implied
      else
        AKind := TNamespaceKind.NeedToWrite;
    end
    else
      AKind := TNamespaceKind.NeedToWrite;
  end;

  AddNamespace(APrefix, ANs, AKind);
end;

function TdxXmlWellFormedWriter.PushNamespaceExplicit(const APrefix, ANs: string): Boolean;
var
  AWriteItOut: Boolean;
  AExistingNsIndex: Integer;
  AExistingNsKind: TNamespaceKind;
  ADefinedNs: string;
begin
  AWriteItOut := True;
  AExistingNsIndex := LookupNamespaceIndex(APrefix);
  if AExistingNsIndex <> -1 then
  begin
    if AExistingNsIndex > FElemScopeStack[FElemTop].PrevNSTop then
    begin
      if FNsStack[AExistingNsIndex].NamespaceUri <> ANs then
        raise EdxXmlException.CreateFmt(SXmlRedefinePrefix, [APrefix, FNsStack[AExistingNsIndex].NamespaceUri, ANs]);
      AExistingNsKind := FNsStack[AExistingNsIndex].kind;
      if AExistingNsKind = TNamespaceKind.Written then
      begin
        if APrefix = '' then
          raise DupAttrException('', 'xmlns')
        else
          raise DupAttrException('xmlns', APrefix);
      end;
      if FOmitDuplNamespaces and (AExistingNsKind <> TNamespaceKind.NeedToWrite) then
        AWriteItOut := False;
      FNsStack[AExistingNsIndex].Kind := TNamespaceKind.Written;
      Exit(AWriteItOut);
    end
    else
    begin
      if FOmitDuplNamespaces and (FNsStack[AExistingNsIndex].NamespaceUri = ANs) then
        AWriteItOut := False;
    end;
  end
  else
  begin
    if FPredefinedNamespaces <> nil then
    begin
      ADefinedNs := FPredefinedNamespaces.LookupNamespace(APrefix);
      if FOmitDuplNamespaces and (ADefinedNs = ANs) then
        AWriteItOut := False;
    end;
  end;
  if ((ANs = TdxXmlReservedNs.NsXml) and (APrefix <> 'xml')) or ((ANs = TdxXmlReservedNs.NsXmlNs) and (APrefix <> 'xmlns')) then
    raise EdxXmlArgumentException.CreateFmt(SXmlNamespaceDeclXmlXmlns, [APrefix]);
  if (APrefix <> '') and (APrefix[1] = 'x') then
  begin
    if APrefix = 'xml' then
    begin
      if ANs <> TdxXmlReservedNs.NsXml then
        raise EdxXmlArgumentException.Create(SXmlXmlPrefix);
    end
    else
      if APrefix = 'xmlns' then
        raise EdxXmlArgumentException.Create(SXmlXmlnsPrefix);
  end;
  AddNamespace(APrefix, ANs, TNamespaceKind.Written);
  Result := AWriteItOut;
end;

procedure TdxXmlWellFormedWriter.AddNamespace(const APrefix, ANs: string; AKind: TNamespaceKind);
var
  ATop, I: Integer;
begin
  Inc(FNsTop);
  ATop := FNsTop;
  if ATop = Length(FNsStack) then
    SetLength(FNsStack, ATop * 2);

  FNsStack[ATop].&Set(APrefix, ANs, AKind);

  if FUseNsHashTable then
    AddToNamespaceHashtable(FNsTop)
  else
    if FNsTop = MaxNamespacesWalkCount then
    begin
      FNsHashTable.Free;
      FNsHashTable := TdxStringIntegerDictionary.Create;
      for I := 0 to FNsTop do
        AddToNamespaceHashtable(I);
      FUseNsHashTable := True;
    end;
end;

procedure TdxXmlWellFormedWriter.AddToNamespaceHashtable(ANamespaceIndex: Integer);
var
  APrefix: string;
  AExistingNsIndex: Integer;
begin
  APrefix := FNsStack[ANamespaceIndex].Prefix;
  if FNsHashTable.TryGetValue(APrefix, AExistingNsIndex) then
    FNsStack[ANamespaceIndex].PrevNsIndex := AExistingNsIndex;
  FNsHashTable.AddOrSetValue(APrefix, ANamespaceIndex);
end;

function TdxXmlWellFormedWriter.LookupNamespaceIndex(const APrefix: string): Integer;
var
  AIndex, I: Integer;
begin
  if FUseNsHashTable then
  begin
    if FNsHashTable.TryGetValue(APrefix, AIndex) then
      Exit(AIndex);
  end
  else
  begin
    for I := FNsTop downto 0 do
      if FNsStack[I].Prefix = APrefix then
        Exit(I);
  end;
  Result := -1;
end;

procedure TdxXmlWellFormedWriter.PopNamespaces(AIndexFrom: Integer; AIndexTo: Integer);
var
  I: Integer;
begin
  Assert(FUseNsHashTable);
  Assert(AIndexFrom <= AIndexTo);
  for I := AIndexTo downto AIndexFrom do
  begin
    Assert(FNsHashTable.ContainsKey(FNsStack[I].Prefix));
    if FNsStack[I].prevNsIndex = -1 then
      FNsHashTable.Remove(FNsStack[I].Prefix)
    else
      FNsHashTable[FNsStack[I].Prefix] := FNsStack[I].PrevNsIndex;
  end;
end;

class function TdxXmlWellFormedWriter.DupAttrException(const APrefix, ALocalName: string): EdxXmlException;
var
  ASb: TStringBuilder;
begin
  ASb := TStringBuilder.Create;
  try
    if APrefix <> '' then
    begin
      ASb.Append(APrefix);
      ASb.Append(':');
    end;
    ASb.Append(ALocalName);
    Result := EdxXmlException.CreateFmt(SXmlDupAttributeName, [ASb.ToString]);
  finally
    ASb.Free;
  end;
end;

procedure TdxXmlWellFormedWriter.AdvanceState(AToken: TToken);
label
  Advance;
var
  ANewState: TState;
begin
  if FCurrentState >= TStates.Closed then
  begin
    if (FCurrentState = TStates.Closed) or (FCurrentState = TStates.Error) then
      raise EdxXmlInvalidOperationException.Create(SXmlClosedOrError)
    else
      raise EdxXmlInvalidOperationException.CreateFmt(SXmlWrongToken, [Ord(AToken), Ord(FCurrentState)]);
  end;
Advance:
  ANewState := FStateTable[(Ord(AToken) shl 4) + Ord(FCurrentState)];
  if ANewState >= TStates.Error then
  begin
    case ANewState of
      TStates.Error:
        ThrowInvalidStateTransition(AToken, FCurrentState);
      TStates.StartContent:
        begin
          StartElementContent;
          ANewState := TStates.Content;
        end;
      TStates.StartContentEle:
        begin
          StartElementContent;
          ANewState := TStates.Element;
        end;
      TStates.StartContentB64:
        begin
          StartElementContent;
          ANewState := TStates.B64Content;
        end;
      TStates.StartDoc:
        begin
          WriteStartDocument;
          ANewState := TStates.Document;
        end;
      TStates.StartDocEle:
        begin
          WriteStartDocument;
          ANewState := TStates.Element;
        end;
      TStates.EndAttrSEle:
        begin
          WriteEndAttribute;
          StartElementContent;
          ANewState := TStates.Element;
        end;
      TStates.EndAttrEEle:
        begin
          WriteEndAttribute;
          StartElementContent;
          ANewState := TStates.Content;
        end;
      TStates.EndAttrSCont:
        begin
          WriteEndAttribute;
          StartElementContent;
          ANewState := TStates.Content;
        end;
      TStates.EndAttrSAttr:
        begin
          WriteEndAttribute;
          ANewState := TStates.Attribute;
        end;
      TStates.PostB64Cont:
        begin
          FWriter.WriteEndBase64;
          FCurrentState := TStates.Content;
          goto Advance;
        end;
      TStates.PostB64Attr:
        begin
          FWriter.WriteEndBase64;
          FCurrentState := TStates.Attribute;
          goto Advance;
        end;
      TStates.PostB64RootAttr:
        begin
          FWriter.WriteEndBase64;
          FCurrentState := TStates.RootLevelAttr;
          goto Advance;
        end;
      TStates.StartFragEle:
        begin
          StartFragment;
          ANewState := TStates.Element;
        end;
      TStates.StartFragCont:
        begin
          StartFragment;
          ANewState := TStates.Content;
        end;
      TStates.StartFragB64:
        begin
          StartFragment;
          ANewState := TStates.B64Content;
        end;
      TStates.StartRootLevelAttr:
        begin
          WriteEndAttribute;
          ANewState := TStates.RootLevelAttr;
        end;
      else
        Assert(False, 'We should not get to this point.');
    end;
  end;

  FCurrentState := ANewState;
end;

procedure TdxXmlWellFormedWriter.StartElementContent;
var
  AStart, I: Integer;
begin
  AStart := FElemScopeStack[FElemTop].prevNSTop;
  for I := FNsTop downto AStart + 1 do
  begin
    if FNsStack[I].kind = TNamespaceKind.NeedToWrite then
      FNsStack[I].WriteDecl(FWriter, FWriter);
  end;

  FWriter.StartElementContent;
end;

function TdxXmlWellFormedWriter.LookupNamespace(const APrefix: string): string;
var
  I: Integer;
begin
  for I := FNsTop downto 0 do
  begin
    if FNsStack[I].Prefix = APrefix then
      Exit(FNsStack[I].NamespaceUri);
  end;
  if FPredefinedNamespaces <> nil then
    Result := FPredefinedNamespaces.LookupNamespace(APrefix)
  else
    Result := '';
end;

function TdxXmlWellFormedWriter.LookupLocalNamespace(const APrefix: string): string;
var
  I: Integer;
begin
  for I := FNsTop downto FElemScopeStack[FElemTop].PrevNSTop + 1 do
    if FNsStack[I].Prefix = APrefix then
      Exit(FNsStack[I].NamespaceUri);
  Result := '';
end;

function TdxXmlWellFormedWriter.GeneratePrefix: string;
var
  AGenPrefix, S: string;
  I: Integer;
begin
  AGenPrefix := 'p' + IntToStr(FNsTop - 2);
  if LookupNamespace(AGenPrefix) = '' then
    Exit(AGenPrefix);

  I := 0;
  repeat
    S := AGenPrefix + IntToStr(I);
    Inc(I);
  until not (LookupNamespace(S) <> '');
  Result := S;
end;

procedure TdxXmlWellFormedWriter.CheckNCName(const ANcname: string);
var
  ALen: Integer;
  P, AStart: PChar;
begin
  Assert(ANcname <> '');
  AStart := PChar(ANcname);
  if (TdxXmlCharType.CharProperties[AStart^] and TdxXmlCharType.NCStartNameSC) = 0 then
    raise InvalidCharsException(ANcname, 0);

  P := AStart + 1;
  ALen := Length(ANcname) - 1;
  while ALen > 0 do
  begin
    if (TdxXmlCharType.CharProperties[P^] and TdxXmlCharType.NCNameSC) <> 0 then
      Inc(P)
    else
      raise InvalidCharsException(ANcname, P - AStart);
    Dec(ALen);
  end;
end;

class function TdxXmlWellFormedWriter.InvalidCharsException(const AName: string; ABadCharIndex: Integer): EdxXmlException;
begin
  Result := EdxXmlException.CreateFmt(SXmlInvalidNameCharsDetail, [AName, ABadCharIndex, CharToHex(AName[ABadCharIndex + 1])]);
end;

function TdxXmlWellFormedWriter.GetIsClosedOrErrorState: Boolean;
begin
  Result := FCurrentState >= TStates.Closed;
end;

procedure TdxXmlWellFormedWriter.AddAttribute(const APrefix, ALocalName, ANamespaceName: string);
var
  I, ATop, APrev: Integer;
begin
  ATop := FAttrCount;
  Inc(FAttrCount);
  if ATop = Length(FAttrStack) then
    SetLength(FAttrStack, ATop * 2);
  FAttrStack[ATop].&Set(APrefix, ALocalName, ANamespaceName);

  if FAttrCount < MaxAttrDuplWalkCount then
  begin
    for I := 0 to ATop - 1 do
      if FAttrStack[I].IsDuplicate(APrefix, ALocalName, ANamespaceName) then
        raise DupAttrException(APrefix, ALocalName);
  end
  else
  begin
    if FAttrCount = MaxAttrDuplWalkCount then
    begin
      if FAttrHashTable = nil then
        FAttrHashTable := TdxStringIntegerDictionary.Create; //TODO: (FHasher);
      Assert(FAttrHashTable.Count = 0);
      for I := 0 to ATop - 1 do
        AddToAttrHashTable(I);
    end;

    AddToAttrHashTable(ATop);
    APrev := FAttrStack[ATop].Prev;
    while APrev > 0 do
    begin
      Dec(APrev);
      if FAttrStack[APrev].IsDuplicate(APrefix, ALocalName, ANamespaceName) then
        raise DupAttrException(APrefix, ALocalName);
      APrev := FAttrStack[APrev].Prev;
    end;
  end;
end;

procedure TdxXmlWellFormedWriter.AddToAttrHashTable(AAttributeIndex: Integer);
var
  ALocalName: string;
  ACount, APrev: Integer;
begin
  ALocalName := FAttrStack[AAttributeIndex].LocalName;
  ACount := FAttrHashTable.Count;
  FAttrHashTable.AddOrSetValue(ALocalName, 0);
  if ACount <> FAttrHashTable.Count then
    Exit;

  APrev := AAttributeIndex - 1;
  while APrev >= 0 do
  begin
    if FAttrStack[APrev].localName = ALocalName then
      Break;
    Dec(APrev);
  end;
  Assert((APrev >= 0) and (FAttrStack[APrev].LocalName = ALocalName));
  FAttrStack[AAttributeIndex].Prev := APrev + 1;
end;

end.
