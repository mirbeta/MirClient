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

unit dxXMLReader;

{$I cxVer.inc}
{$SCOPEDENUMS ON}

interface

{$IFDEF DELPHIXE}
uses
  Classes, SysUtils, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses, dxStringHelper, dxXMLClasses;

type
  TdxXmlReader = class;

  TdxXmlReadState = (
    Initial,
    Interactive,
    Error,
    EndOfFile,
    Closed
  );

  TdxXmlNodeType = (
    None,
    Element,
    Attribute,
    Text,
    CDATA,
    EntityReference,
    Entity,
    ProcessingInstruction,
    Comment,
    Document,
    DocumentType,
    DocumentFragment,
    Notation,
    Whitespace,
    SignificantWhitespace,
    EndElement,
    EndEntity,
    XmlDeclaration
  );

  TdxXmlSpace = (
    None,
    Default,
    Preserve
  );

  TdxWhitespaceHandling = (
     All,
     Significant,
     None
  );

  { TdxXmlNameTable }

  TdxXmlNameTable = class
  private
    FEntries: TdxStringSet;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AKey: string): string; overload;
    function Add(const AKey: TCharArray; AStart: Integer; ALen: Integer): string; overload;
    function Get(const AValue: string): string; overload;
    function Get(const AKey: TCharArray; AStart: Integer; ALen: Integer): string; overload;
  end;

  { TdxXmlReaderSettings }

  TdxXmlReaderSettings = class
  strict private
    FNameTable: TdxXmlNameTable;
    FLineNumberOffset: Integer;
    FLinePositionOffset: Integer;
    FConformanceLevel: TdxXmlConformanceLevel;
    FCheckCharacters: Boolean;
    FMaxCharactersInDocument: Int64;
    FIgnoreWhitespace: Boolean;
    FIgnorePIs: Boolean;
    FIgnoreComments: Boolean;
    FCloseInput: Boolean;
  protected
    procedure Initialize; overload;
  public
    constructor Create; overload;
    function CreateReader(AInput: TStream): TdxXmlReader; overload;

    property NameTable: TdxXmlNameTable read FNameTable write FNameTable;
    property LineNumberOffset: Integer read FLineNumberOffset write FLineNumberOffset;
    property LinePositionOffset: Integer read FLinePositionOffset write FLinePositionOffset;
    property ConformanceLevel: TdxXmlConformanceLevel read FConformanceLevel write FConformanceLevel;
    property CheckCharacters: Boolean read FCheckCharacters write FCheckCharacters;
    property MaxCharactersInDocument: Int64 read FMaxCharactersInDocument write FMaxCharactersInDocument;
    property IgnoreWhitespace: Boolean read FIgnoreWhitespace write FIgnoreWhitespace;
    property IgnoreProcessingInstructions: Boolean read FIgnorePIs write FIgnorePIs;
    property IgnoreComments: Boolean read FIgnoreComments write FIgnoreComments;
    property CloseInput: Boolean read FCloseInput write FCloseInput;
  end;

  { TdxXmlReader }

  TdxXmlReader = class
  strict private
    function SkipSubtree: Boolean;
  private const
    DefaultBufferSize = 4096;
    BiggerBufferSize = 8192;
    MaxStreamLengthForDefaultBufferSize = 64 * 1024;
    HasValueBitmap = $02659C;
  private
    FReadState: TdxXmlReadState;
    class function HasValueInternal(ANodeType: TdxXmlNodeType): Boolean; static;
  protected
    class function CalcBufferSize(AInput: TStream): Integer; static;

    function GetDepth: Integer; virtual; abstract;
    function GetHasValue: Boolean; virtual;
    function GetName: string; virtual;
    function GetNamespaceURI: string; virtual; abstract;
    function GetPrefix: string; virtual; abstract;
    function GetSettings: TdxXmlReaderSettings; virtual;
    function GetValue: string; virtual; abstract;
    function GetNameTable: TdxXmlNameTable; virtual; abstract;
    function GetNodeType: TdxXmlNodeType; virtual; abstract;
    function GetLocalName: string; virtual; abstract;
    function GetXmlSpace: TdxXmlSpace; virtual;

    property NameTable: TdxXmlNameTable read GetNameTable;
  public
    function Read: Boolean; virtual; abstract;
    function GetAttribute(const AAttribute: string): string; overload; virtual; abstract;
    function GetAttribute(const AAttribute, ANamespaceURI: string): string; overload; virtual; abstract;
    function IsEmptyElement: Boolean; virtual; abstract;
    function LookupNamespace(const ANameSpace: string): string; overload; virtual; abstract;
    function MoveToElement: Boolean; virtual; abstract;
    function MoveToNextAttribute: Boolean; virtual; abstract;
    function ReadToFollowing(const ALocalName: string): Boolean; overload; virtual;
    function ReadToFollowing(const ALocalName, ANameSpaceURI: string): Boolean; overload; virtual;
    procedure Skip; virtual;

    property Settings: TdxXmlReaderSettings read GetSettings;

    property LocalName: string read GetLocalName;
    property HasValue: Boolean read GetHasValue;
    property Name: string read GetName;
    property NamespaceURI: string read GetNamespaceURI;
    property NodeType: TdxXmlNodeType read GetNodeType;
    property Prefix: string read GetPrefix;
    property ReadState: TdxXmlReadState read FReadState;
    property XmlSpace: TdxXmlSpace read GetXmlSpace;
    property Value: string read GetValue;
    property Depth: Integer read GetDepth;
  end;

{$ENDIF}
implementation

{$IFDEF DELPHIXE}
uses
  Math, StrUtils, Windows;

resourcestring
  SXmlBadAttributeChar = '''%s'', hexadecimal value %s, is an invalid attribute character.';
  SXmlBadDecimalEntity = 'Invalid syntax for a decimal numeric entity reference.';
  SXmlBadDTDLocation = 'Unexpected DTD declaration.';
  SXmlBadHexEntity = 'Invalid syntax for a hexadecimal numeric entity reference.';
  SXmlBadNameChar = 'The ''%s'' character, hexadecimal value %s, cannot be included in a name.';
  SXmlBadNamespaceDecl = 'Invalid namespace declaration.';
  SXmlBadStartNameChar = 'Name cannot begin with the ''%s'' character, hexadecimal value %s.';
  SXmlCDATAEndInText = ''']]&gt;'' is not allowed in character data.';
  SXmlCharEntityOverflow = 'Invalid value of a character entity reference.';
  SXmlEncodingSwitchAfterResetState = '''%s'' is an invalid value for the ''encoding'' attribute. ' +
    'The encoding cannot be switched after a call to ResetState';
  SXmlExpectExternalOrClose = 'Expecting external ID, ''['' or ''&gt;''.';
  SXmlExpectSubOrClose = 'Expecting an internal subset or the end of the DOCTYPE declaration.';
  SXmlExpectingWhiteSpace = '''%s'' is an unexpected token. Expecting white space.';
  SXmlInternalError = 'An internal error has occurred.';
  SXmlInvalidCharacter = '''%s'', hexadecimal value %s, is an invalid character.';
  SXmlInvalidCharInThisEncoding = 'Invalid character in the given encoding.';
  SXmlInvalidCommentChars = 'An XML comment cannot contain ''--'', and ''-'' cannot be the last character.';
  SXmlInvalidNodeType = '''%s'' is an invalid XmlNodeType.';
  SXmlInvalidOperation = 'Operation is not valid due to the current state of the object.';
  SXmlInvalidPIName = '''%s'' is an invalid name for processing instructions.';
  SXmlInvalidRootData = 'Data at the root level is invalid.';
  SXmlInvalidTextDecl = 'Invalid text declaration.';
  SXmlInvalidVersionNumber = 'Version number ''%s'' is invalid.';
  SXmlInvalidXmlDecl = 'Syntax for an XML declaration is invalid.';
  SXmlInvalidXmlSpace = '''%s'' is an invalid xml:space value.';
  SXmlLimitExceeded = 'The input document has exceeded a limit set by %s.';
  SXmlMissingRoot = 'Root element is missing.';
  SXmlMultipleRoots = 'There are multiple root elements.';
  SXmlNamespaceDeclXmlXmlns = 'Prefix ''&s'' cannot be mapped to namespace name reserved for ''xml'' or ''xmlns''.';
  SXmlReadOnlyProperty = 'The ''%s'' property is read only and cannot be set.';
  SXmlTagMismatchEx = 'The ''%s'' start tag on line ''%s'' position ''%s'' does not match the end tag of ''%s''.';
  SXmlUnclosedQuote = 'There is an unclosed literal string.';
  SXmlUnexpectedEndTag = 'Unexpected end tag.';
  SXmlUnexpectedEOF = 'Unexpected end of file while parsing %s has occurred.';
  SXmlUnexpectedEOF1 = 'Unexpected end of file has occurred.';
  SXmlUnexpectedEOFInElementContent = 'Unexpected end of file has occurred. The following elements are not closed: %s';
  SXmlUnexpectedTokenEx = '''%s'' is an unexpected token. The expected token is ''%s''.';
  SXmlUnexpectedTokens2 = '''%s'' is an unexpected token. The expected token is ''%s'' or ''%s''.';
  SXmlUnknownNs = '''%s'' is an undeclared namespace.';
  SXmlXmlDeclNotFirst = 'Unexpected XML declaration. The XML declaration must be the first node in the document, ' +
    'and no white space characters are allowed to appear before it.';
  SXmlXmlnsPrefix = 'Prefix ''xmlns'' is reserved for use by XML.';
  SXmlXmlPrefix = 'Prefix ''xml'' is reserved for use by XML and can be mapped only to namespace name ' +
    '''http://www.w3.org/XML/1998/namespace''.';
  SDTDNotImplemented = 'DTD not implemented';

type

  { TEncodingHelper }

  TEncodingHelper = class helper for TEncoding
  public
    procedure Convert(const ABytes: TBytes; AByteIndex, AByteCount: Integer;
      const AChars: TCharArray; ACharIndex, ACharCount: Integer; out ABytesUsed, ACharsUsed: Integer); overload;
    function WebName: string;
  end;

  EdxArgumentOutOfRangeException = class(Exception);

  { TdxXmlNamespaceManager }

  TdxXmlNamespaceManager = class
  protected
    type
      TNamespaceDeclaration = record
        Prefix: string;
        Uri: string;
        ScopeId: Integer;
        PreviousNsIndex: Integer;
        procedure &Set(const APrefix, AUri: string; AScopeId, APreviousNsIndex: Integer);
      end;
    const
      MinDeclsCountForHashTable = 16;
  strict private
    FNsdecls: TArray<TNamespaceDeclaration>;
    FLastDecl: Integer;
    FNameTable: TdxXmlNameTable;
    FScopeId: Integer;
    FHashTable: TDictionary<string, Integer>;
    FUseHashTable: Boolean;
    FXml: string;
    FXmlNs: string;
  protected
    function GetNameTable: TdxXmlNameTable; virtual;
    function GetDefaultNamespace: string; virtual;
  public
    constructor Create(ANameTable: TdxXmlNameTable);
    destructor Destroy; override;
    procedure PushScope; virtual;
    function PopScope: Boolean; virtual;
    procedure AddNamespace(APrefix, AUri: string); virtual;
    procedure RemoveNamespace(const APrefix: string; const AUri: string); virtual;
    function LookupNamespace(const APrefix: string): string; virtual;
    function LookupNamespaceDecl(const APrefix: string): Integer;
    function LookupPrefix(const AUri: string): string; virtual;
    function HasNamespace(const APrefix: string): Boolean; virtual;
    function GetNamespaceDeclaration(AIdx: Integer; out APrefix: string; out AUri: string): Boolean;

    property NameTable: TdxXmlNameTable read GetNameTable;
    property DefaultNamespace: string read GetDefaultNamespace;
  end;

  { TdxLineInfo }

  TdxLineInfo = record
  strict private
    FLineNo: Integer;
    FLinePos: Integer;
  public
    constructor Create(ALineNo: Integer; ALinePos: Integer);
    procedure &Set(ALineNo: Integer; ALinePos: Integer);
    property LineNo: Integer read FLineNo;
    property LinePos: Integer read FLinePos;
  end;

  { TdxNodeData }

  TdxNodeData = class(TcxIUnknownObject, IComparable)
  strict private
    type
      TValueLocation = (CharsBuffer, ValueString);
    class var
      FNone: TdxNodeData;
  strict private
    FType: TdxXmlNodeType;
    FLocalName: string;
    FPrefix: string;
    FNamespace: string;
    FNameWPrefix: string;
    FValueLocation: TValueLocation;
    FValue: string;
    FChars: TCharArray;
    FValueStartPos: Integer;
    FValueLength: Integer;
    FLineInfo: TdxLineInfo;
    FLineInfo2: TdxLineInfo;
    FQuoteChar: Char;
    FDepth: Integer;
    FIsEmptyOrDefault: Boolean;
    FXmlContextPushed: Boolean;
    class function GetNone: TdxNodeData; static;
    function GetLineNo: Integer;
    function GetLinePos: Integer;
    function GetIsEmptyElement: Boolean;
    procedure SetIsEmptyElement(const AValue: Boolean);
    function GetIsDefaultAttribute: Boolean;
    procedure SetIsDefaultAttribute(const AValue: Boolean);
    function GetValueBuffered: Boolean;
    function GetStringValue: string;
    procedure ClearName;
    function CreateNameWPrefix(AXmlNameTable: TdxXmlNameTable): string;
    function CompareTo(AObject: TObject): Integer;
  protected
    class property None: TdxNodeData read GetNone;
    procedure Clear(AType: TdxXmlNodeType);
    procedure CopyTo(AValueOffset: Integer; ASb: TStringBuilder); overload;
    function GetNameWPrefix(ANameTable: TdxXmlNameTable): string;
    procedure SetLineInfo(ALineNo, ALinePos: Integer);
    procedure SetLineInfo2(ALineNo, ALinePos: Integer);
    procedure SetValue(const AValue: string); overload;
    procedure SetValue(const AChars: TCharArray; AStartPos, ALength: Integer); overload;
    procedure SetNamedNode(AType: TdxXmlNodeType; const ALocalName: string); overload;
    procedure SetNamedNode(AType: TdxXmlNodeType; const ALocalName, APrefix, ANameWPrefix: string); overload;
    procedure SetValueNode(AType: TdxXmlNodeType; const AValue: string); overload;
    procedure SetValueNode(AType: TdxXmlNodeType; const AChars: TCharArray; AStartPos, ALength: Integer); overload;
    procedure OnBufferInvalidated;

    property Depth: Integer read FDepth write FDepth;
    property QuoteChar: Char read FQuoteChar write FQuoteChar;
    property LineInfo: TdxLineInfo read FLineInfo write FLineInfo;
    property LineInfo2: TdxLineInfo read FLineInfo2;
    property LocalName: string read FLocalName;
    property Namespace: string read FNamespace write FNamespace;
    property Prefix: string read FPrefix;
    property &Type: TdxXmlNodeType read FType write FType;
    property XmlContextPushed: Boolean read FXmlContextPushed write FXmlContextPushed;
  public
    constructor Create;

    property LineNo: Integer read GetLineNo;
    property LinePos: Integer read GetLinePos;
    property IsEmptyElement: Boolean read GetIsEmptyElement write SetIsEmptyElement;
    property IsDefaultAttribute: Boolean read GetIsDefaultAttribute write SetIsDefaultAttribute;
    property ValueBuffered: Boolean read GetValueBuffered;
    property StringValue: string read GetStringValue;
  end;

  { TdxXmlTextReaderImpl }

  TdxXmlTextReaderImpl = class(TdxXmlReader)
  private const
    MaxBytesToMove = 128;
    ApproxXmlDeclLength = 80;
    NodesInitialSize = 8;
    MaxByteSequenceLen = 6;
    MaxAttrDuplWalkCount = 250;
    MinWhitespaceLookahedCount = 4096;
    XmlDeclarationBeginning = '<?xml';
    MaxUTF8EncodedCharByteCount = 6;
  private type
{$REGION 'Private helper types'}
    TParsingFunction = (
      ElementContent,
      NoData,
      SwitchToInteractive,
      SwitchToInteractiveXmlDecl,
      DocumentContent,
      MoveToElementContent,
      PopElementContext,
      PopEmptyElementContext,
      ResetAttributesRootLevel,
      Error,
      Eof,
      ReaderClosed,
      EntityReference,
      InIncrementalRead,
      FragmentAttribute,
      ReportEndEntity,
      AfterResolveEntityInContent,
      AfterResolveEmptyEntityInContent,
      XmlDeclarationFragment,
      GoToEof,
      PartialTextValue,

      InReadAttributeValue,
      InReadValueChunk,
      InReadContentAsBinary,
      InReadElementContentAsBinary
    );

    TParsingMode = (
      Full,
      SkipNode,
      SkipContent
    );

    TEntityType = (
      CharacterDec,
      CharacterHex,
      CharacterNamed,
      Expanded,
      Skipped,
      FakeExpanded,
      Unexpanded,
      ExpandedInAttribute
    );

    TEntityExpandType = (
      All,
      OnlyGeneral,
      OnlyCharacter
    );

    TLaterInitParam = class
    public
      InputStream: TStream;
      InputBytes: TBytes;
      InputByteCount: Integer;
    end;

    TXmlContext = class
    public
      XmlSpace: TdxXmlSpace;
      XmlLang: string;
      DefaultNamespace: string;
      PreviousContext: TXmlContext;
      constructor Create(APreviousContext: TXmlContext); overload;
    end;

    TParsingState = record
    strict private
      function GetLinePos: Integer;
    public
      Chars: TCharArray;
      CharPos: Integer;
      CharsUsed: Integer;
      Encoding: TEncoding;
      AppendMode: Boolean;
      Stream: TStream;
      Decoder: TEncoding;
      Bytes: TBytes;
      BytePos: Integer;
      BytesUsed: Integer;
      LineNo: Integer;
      LineStartPos: Integer;
      IsEof: Boolean;
      IsStreamEof: Boolean;
      EolNormalized: Boolean;
      procedure Clear;

      property LinePos: Integer read GetLinePos;
    end;

{$ENDREGION}
  private
    FXML: string;
    FXmlNs: string;

    FLaterInitParam: TLaterInitParam;

    FParsingState: TParsingState;

    FParsingFunction: TParsingFunction;
    FNextParsingFunction: TParsingFunction;
    FNodes: TArray<TdxNodeData>;
    FCurrentNode: TdxNodeData;
    FIndex: Integer;
    FCurrentAttributeIndex: Integer;
    FAttributeCount: Integer;
    FAttributeHashTable: Integer;
    FAttributeDuplicateWalkCount: Integer;
    FAttributeNeedNamespaceLookup: Boolean;
    FFullAttributeCleanup: Boolean;
    FInternalNameTable: TdxXmlNameTable;
    FNameTable: TdxXmlNameTable;
    FNameTableFromSettings: Boolean;

    FNormalize: Boolean;
    FSupportNamespaces: Boolean;
    FWhitespaceHandling: TdxWhitespaceHandling;
    FIgnorePIs: Boolean;
    FIgnoreComments: Boolean;
    FCheckCharacters: Boolean;
    FLineNumberOffset: Integer;
    FLinePositionOffset: Integer;

    FCloseInput: Boolean;
    FMaxCharactersInDocument: Int64;

    FNamespaceManager: TdxXmlNamespaceManager;
    FLastPrefix: string;

    FXmlContext: TXmlContext;

    FFragmentType: TdxXmlNodeType;
    FFragment: Boolean;

    FStringBuilder: TStringBuilder;
    FRootElementParsed: Boolean;
    FStandalone: Boolean;
    FParsingMode: TParsingMode;
    FAfterResetState: Boolean;
    FDocumentStartBytePos: Integer;
    FReadValueOffset: Integer;
    FCharactersInDocument: Int64;
    FCharactersFromEntities: Int64;

    function DetectEncoding: TEncoding;

    function GetIndexOfAttributeWithoutPrefix(const AName: string): Integer;
    function GetIndexOfAttributeWithPrefix(const AName: string): Integer;
    function GetInAttributeValueIterator: Boolean;
    function GetChars(AMaxCharsCount: Integer): Integer;

    procedure OnEof;

    procedure InitStreamInput(AStream: TStream; const ABytes: TBytes;
      AByteCount: Integer; AEncoding: TEncoding);
    procedure FinishInitStream;

    function AddAttribute(AEndNamePos, AColonPos: Integer): TdxNodeData; overload;
    function AddAttribute(const ALocalName, APrefix: string; const ANameWPrefix: string): TdxNodeData; overload;
    function AddAttributeNoChecks(const AName: string; AAttrDepth: Integer): TdxNodeData;
    function AddNode(ANodeIndex, ANodeDepth: Integer): TdxNodeData;
    function AllocNode(ANodeIndex, ANodeDepth: Integer): TdxNodeData;
    function GetTextNodeType(AOrChars: Integer): TdxXmlNodeType;
    function GetWhitespaceType: TdxXmlNodeType;
    function LookupNamespace(ANode: TdxNodeData): string; overload;
    procedure AddNamespace(const APrefix, AUri: string; AAttribute: TdxNodeData);
    procedure AttributeNamespaceLookup;
    procedure ElementNamespaceLookup;
    procedure InvalidCharRecovery(var ABytesCount: Integer; out ACharsCount: Integer);
    procedure OnDefaultNamespaceDecl(AAttribute: TdxNodeData);
    procedure OnNamespaceDecl(AAttribute: TdxNodeData);
    procedure OnXmlReservedAttribute(AAttribute: TdxNodeData);
    procedure ParseAttributeValueSlow(ACurPosition: Integer; AQuoteChar: Char; AAttribute: TdxNodeData);
    procedure PopXmlContext;
    procedure PushXmlContext;

    function ParseCDataOrComment(AType: TdxXmlNodeType; out AOutStartPosition, AOutEndPosition: Integer): Boolean; overload;
    function ParseCharRefInline(AStartPosition: Integer; out ACharCount: Integer; out AEntityType: TEntityType): Integer;
    function ParseComment: Boolean;
    function ParseDocumentContent: Boolean;
    function ParseElementContent: Boolean;
    function ParseName: Integer;
    function ParseNamedCharRef(AExpand: Boolean; AInternalSubsetBuilder: TStringBuilder): Integer;
    function ParseNamedCharRefInline(AStartPosition: Integer; AExpand: Boolean; AInternalSubsetBuilder: TStringBuilder): Integer;
    function ParseNumericCharRef(AExpand: Boolean; AInternalSubsetBuilder: TStringBuilder; out AEntityType: TEntityType): Integer;
    function ParseNumericCharRefInline(AStartPosition: Integer; AExpand: Boolean; AInternalSubsetBuilder: TStringBuilder;
      out ACharCount: Integer; out AEntityType: TEntityType): Integer;
    function ParsePI(APiInDtdStringBuilder: TStringBuilder = nil): Boolean;
    function ParsePIValue(out AOutStartPosition, AOutEndPosition: Integer): Boolean;
    function ParseQName(AIsQName: Boolean; AStartOffset: Integer; out AColonPosition: Integer): Integer; overload;
    function ParseQName(out AColonPosition: Integer): Integer; overload;
    function ParseRootLevelWhitespace: Boolean;
    function ParseText: Boolean; overload;
    function ParseText(out AStartPosition, AEndPosition: Integer; var AOutOrChars: Integer): Boolean; overload;
    function ParseUnexpectedToken: string; overload;
    function ParseUnexpectedToken(APosition: Integer): string; overload;
    function ParseXmlDeclaration(AIsTextDecl: Boolean): Boolean;
    function ReadDataInName(var APosition: Integer): Boolean;
    procedure ParseAttributes;
    procedure ParseCData;
    procedure ParseCDataOrComment(AType: TdxXmlNodeType); overload;
    procedure ParseElement;
    procedure ParseEndElement;
    procedure ParseXmlDeclarationFragment;
    procedure SkipPartialTextValue;

    function HandleEntityReference(AIsInAttributeValue: Boolean; AExpandType: TEntityExpandType;
      out ACharRefEndPos: Integer): TEntityType;
    procedure PopElementContext;
    procedure ResetAttributes;
    procedure FullAttributeCleanup; inline;
    procedure FinishPartialValue;

    function ReadData: Integer;
    procedure RegisterConsumedCharacters(ACharacters: Int64);
    function CheckEncoding(const ANewEncodingName: string): TEncoding;
    procedure SetupEncoding(AEncoding: TEncoding);
    procedure SwitchEncoding(ANewEncoding: TEncoding);
    procedure SwitchEncodingToUTF8;

    procedure ReThrow(E: Exception; ALineNo, ALinePos: Integer);
    procedure SetErrorState;
    procedure Throw(E: Exception); overload;
    procedure Throw(const ARes: string); overload;
    procedure Throw(const ARes, AArg: string); overload;
    procedure Throw(const ARes: string; const AArgs: array of const); overload;
    procedure Throw(const ARes: string; ALineNo, ALinePos: Integer); overload;
    procedure Throw(const ARes, AArg: string; ALineNo, ALinePos: Integer); overload;
    procedure Throw(APosition: Integer; const ARes: string); overload;
    procedure Throw(APosition: Integer; const ARes, AArg: string); overload;
    procedure Throw(APosition: Integer; const ARes: string; const AArgs: array of const); overload;
    procedure Throw(APosition: Integer; const ARes: string; const AArgs: TArray<string>); overload;
    procedure ThrowExpectingWhitespace(APosition: Integer);
    procedure ThrowInvalidChar(const AData: TCharArray; ALength, AInvCharPos: Integer);
    procedure ThrowTagMismatch(AStartTag: TdxNodeData);
    procedure ThrowUnexpectedToken(APosition: Integer; const AExpectedToken1: string;
      const AExpectedToken2: string = ''); overload;
    procedure ThrowUnexpectedToken(const AExpectedToken1, AExpectedToken2: string); overload;
    procedure ThrowUnexpectedToken(AExpectedToken: string); overload;
    procedure ThrowUnclosedElements;
    procedure ThrowWithoutLineInfo(const ARes: string); overload;
    procedure ThrowWithoutLineInfo(const ARes, AArg: string); overload;

    procedure OnNewLine(APosition: Integer);
    function EatWhitespaces(ASb: TStringBuilder): Integer;
    procedure ShiftBuffer(ASourcePosition, ADestPosition, ACount: Integer);
    procedure UnDecodeChars;
  protected
    constructor Create(ASettings: TdxXmlReaderSettings); overload;
    class procedure BlockCopyChars(ASource: TCharArray; ASourceOffset: Integer; ADestination: TCharArray;
      ADestinationOffset, ACount: Integer); static; inline;
    class function ConvertToConstArray(const AArgs: TArray<string>): TArray<TVarRec>;
    class function StrEqual(const AChars: TCharArray; AStrPos1, AStrLen1: Integer; const AStr2: string): Boolean; static;
    class function StripSpaces(const AValue: string): string; overload; static;
    class procedure StripSpaces(var AValue: TCharArray; AIndex: Integer; var ALen: Integer); overload; static;

    procedure ClearNodes;
    procedure FinishInit;
    function GetNameTable: TdxXmlNameTable; override;
    function GetNodeType: TdxXmlNodeType; override;
    function GetLocalName: string; override;
    function GetNamespaceURI: string; override;
    function GetValue: string; override;
    function GetDepth: Integer; override;
    function GetXmlSpace: TdxXmlSpace; override;
    function GetPrefix: string; override;

    property InAttributeValueIterator: Boolean read GetInAttributeValueIterator;
    property XML: string read FXML;
    property XmlNs: string read FXmlNs;
  public
    constructor Create; overload;
    constructor Create(AStream: TStream; const ABytes: TBytes; AByteCount: Integer;
      ASettings: TdxXmlReaderSettings; ACloseInput: Boolean); overload;
    destructor Destroy; override;
    function GetAttribute(const AName: string): string; overload; override;
    function GetAttribute(const AAttribute, ANamespaceURI: string): string; overload; override;
    function IsEmptyElement: Boolean; override;
    function LookupNamespace(const APrefix: string): string; overload; override;
    function MoveToElement: Boolean; override;
    function MoveToNextAttribute: Boolean; override;
    function Read: Boolean; override;
  end;

function NotImplemented: Pointer;
begin
  raise EdxException.Create('Not implemented');
end;

function IfThen(AValue: Boolean; const ATrue: Boolean; const AFalse: Boolean = False): Boolean; overload; inline;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function GetRemainingUTF8EncodedCharacterByteCount(ABuffer: PByte; ABytesInBuffer: Integer): Integer;
begin
  if ABytesInBuffer = 0 then
    Exit(0);

  Inc(ABuffer, ABytesInBuffer - 1);

  if ABuffer^ and $80 = $00 then
    Exit(0);

  if ABytesInBuffer > 1 then
  begin
    if ABuffer^ and $E0 = $C0 then
      Exit(1);
    if ABuffer^ and $F0 = $E0 then
      Exit(2);
    if ABuffer^ and $F8 = $F0 then
      Exit(3);
    if ABuffer^ and $FC = $F8 then
      Exit(4);
    if ABuffer^ and $FE = $FC then
      Exit(5);
  end;
  if ABytesInBuffer > 2 then
  begin
    Dec(ABuffer);
    if ABuffer^ and $F0 = $E0 then
      Exit(1);
    if ABuffer^ and $F8 = $F0 then
      Exit(2);
    if ABuffer^ and $FC = $F8 then
      Exit(3);
    if ABuffer^ and $FE = $FC then
      Exit(4);
  end;
  if ABytesInBuffer > 3 then
  begin
    Dec(ABuffer);
    if ABuffer^ and $F8 = $F0 then
      Exit(1);
    if ABuffer^ and $FC = $F8 then
      Exit(2);
    if ABuffer^ and $FE = $FC then
      Exit(3);
  end;
  if ABytesInBuffer > 4 then
  begin
    Dec(ABuffer);
    if ABuffer^ and $FC = $F8 then
      Exit(1);
    if ABuffer^ and $FE = $FC then
      Exit(2);
  end;
  if ABytesInBuffer > 5 then
  begin
    Dec(ABuffer);
    if ABuffer^ and $FE = $FC then
      Exit(1);
  end;
  Result := 0;
end;

{ TEncodingHelper }

procedure TEncodingHelper.Convert(const ABytes: TBytes; AByteIndex, AByteCount: Integer; const AChars: TCharArray;
  ACharIndex, ACharCount: Integer; out ABytesUsed, ACharsUsed: Integer);
var
  ACharArray: TCharArray;
begin
  AByteCount := Min(AByteCount, GetMaxByteCount(ACharCount));
  if Self = UTF8 then
  begin
    while (AByteCount > 0) and
      (GetRemainingUTF8EncodedCharacterByteCount(@ABytes[AByteIndex], AByteCount) <> 0) do
        Dec(AByteCount);
  end
  else

    if Self is TMBCSEncoding then
    begin
      while (AByteCount > 0) and
        (UnicodeFromLocaleChars(TMBCSEncoding(Self).CodePage, MB_ERR_INVALID_CHARS, PAnsiChar(@ABytes[AByteIndex]), AByteCount, nil, 0) = 0) do
          Dec(AByteCount);
    end;

  ACharArray := GetChars(ABytes, AByteIndex, AByteCount);
  ACharsUsed := Min(Length(ACharArray), ACharCount);
  Move(ACharArray[0], AChars[ACharIndex], ACharsUsed * SizeOf(Char));
  ABytesUsed := GetByteCount(ACharArray, 0, ACharsUsed);
end;

function TEncodingHelper.WebName: string;
var
  AStartPos: Integer;
  AEncodingName: string;
begin
  AEncodingName := EncodingName;
  AStartPos := Pos('(', AEncodingName) + 1;
  Result := Copy(AEncodingName, AStartPos, Pos(')', AEncodingName) - AStartPos);
  Result := LowerCase(Result);
end;

{ TdxXmlReaderSettings }

constructor TdxXmlReaderSettings.Create;
begin
  inherited Create;
  Initialize;
end;

function TdxXmlReaderSettings.CreateReader(AInput: TStream): TdxXmlReader;
begin
  if AInput = nil then
    raise EdxXmlArgumentNullException.Create('input');
  Result := TdxXmlTextReaderImpl.Create(AInput, nil, 0, Self, FCloseInput);
end;

procedure TdxXmlReaderSettings.Initialize;
begin
  FNameTable := nil;

  FLineNumberOffset := 0;
  FLinePositionOffset := 0;
  FCheckCharacters := True;
  FConformanceLevel := TdxXMLConformanceLevel.Document;
  FIgnoreWhitespace := False;
  FIgnorePIs := False;
  FIgnoreComments := False;
  FCloseInput := False;
  FMaxCharactersInDocument := 0;
end;

{ TdxXmlNamespaceManager.TNamespaceDeclaration }

procedure TdxXmlNamespaceManager.TNamespaceDeclaration.&Set(const APrefix, AUri: string; AScopeId, APreviousNsIndex: Integer);
begin
  Prefix := APrefix;
  Uri := AUri;
  ScopeId := AScopeId;
  PreviousNsIndex := APreviousNsIndex;
end;

{ TdxXmlNamespaceManager }

constructor TdxXmlNamespaceManager.Create(ANameTable: TdxXmlNameTable);
var
  AEmptyStr: string;
begin
  FLastDecl := 0;
  FNameTable := ANameTable;
  FXml := ANameTable.Add('xml');
  FXmlNs := ANameTable.Add('xmlns');

  SetLength(FNsdecls, 8);
  AEmptyStr := ANameTable.Add('');
  FNsdecls[0].&Set(AEmptyStr, AEmptyStr, -1, -1);
  FNsdecls[1].&Set(FXmlNs, ANameTable.Add(TdxXmlReservedNs.NsXmlNs), -1, -1);
  FNsdecls[2].&Set(FXml, ANameTable.Add(TdxXmlReservedNs.NsXml), 0, -1);
  FLastDecl := 2;
  FScopeId := 1;
end;

destructor TdxXmlNamespaceManager.Destroy;
begin
  FHashTable.Free;
  inherited Destroy;
end;

function TdxXmlNamespaceManager.GetNameTable: TdxXmlNameTable;
begin
  Result := FNameTable;
end;

function TdxXmlNamespaceManager.GetDefaultNamespace: string;
begin
  Result := LookupNamespace('');
end;

procedure TdxXmlNamespaceManager.PushScope;
begin
  Inc(FScopeId);
end;

function TdxXmlNamespaceManager.PopScope: Boolean;
var
  ADecl: Integer;
begin
  ADecl := FLastDecl;
  if FScopeId = 1 then
    Exit(False);
  while FNsdecls[ADecl].ScopeId = FScopeId do
  begin
    if FUseHashTable then
      FHashTable[FNsdecls[ADecl].Prefix] := FNsdecls[ADecl].PreviousNsIndex;
    Dec(ADecl);
    Assert(ADecl >= 2);
  end;
  FLastDecl := ADecl;
  Dec(FScopeId);
  Result := True;
end;

procedure TdxXmlNamespaceManager.AddNamespace(APrefix, AUri: string);
var
  ADeclIndex, APreviousDeclIndex, I: Integer;
begin
  APrefix := FNameTable.Add(APrefix);
  AUri := FNameTable.Add(AUri);
  if (Pointer(FXml) = Pointer(APrefix)) and (AUri <> TdxXmlReservedNs.NsXml) then
    raise EdxXmlArgumentException.Create(SXmlXmlPrefix);
  if Pointer(FXmlNs) = Pointer(APrefix) then
    raise EdxXmlArgumentException.Create(SXmlXmlnsPrefix);

  ADeclIndex := LookupNamespaceDecl(APrefix);
  APreviousDeclIndex := -1;
  if ADeclIndex <> -1 then
  begin
    if FNsdecls[ADeclIndex].ScopeId = FScopeId then
    begin
      FNsdecls[ADeclIndex].Uri := AUri;
      Exit;
    end
    else
      APreviousDeclIndex := ADeclIndex;
  end;
  if FLastDecl = Length(FNsdecls) - 1 then
    SetLength(FNsdecls, Length(FNsdecls) * 2);

  Inc(FLastDecl);
  FNsdecls[FLastDecl].&Set(APrefix, AUri, FScopeId, APreviousDeclIndex);
  if FUseHashTable then
    FHashTable.AddOrSetValue(APrefix, FLastDecl)
  else
    if FLastDecl >= MinDeclsCountForHashTable then
    begin
      Assert(FHashTable = nil);
      FHashTable := TDictionary<string, Integer>.Create(FLastDecl);
      for I := 0 to FLastDecl do
        FHashTable.AddOrSetValue(FNsdecls[I].Prefix, I);
      FUseHashTable := True;
    end;
end;

procedure TdxXmlNamespaceManager.RemoveNamespace(const APrefix: string; const AUri: string);
var
  ADeclIndex: Integer;
begin
  if AUri = '' then
    raise EdxXmlArgumentException.Create('uri');
  if APrefix = '' then
    raise EdxXmlArgumentException.Create('prefix');

  ADeclIndex := LookupNamespaceDecl(APrefix);
  while ADeclIndex <> -1 do
  begin
    if (FNsdecls[ADeclIndex].ScopeId = FScopeId) and (FNsdecls[ADeclIndex].Uri = AUri) then
      FNsdecls[ADeclIndex].Uri := '';
    ADeclIndex := FNsdecls[ADeclIndex].PreviousNsIndex;
  end;
end;

function TdxXmlNamespaceManager.LookupNamespace(const APrefix: string): string;
var
  ADeclIndex: Integer;
begin
  ADeclIndex := LookupNamespaceDecl(APrefix);
  if (ADeclIndex = -1) then
    Result := ''
  else
    Result := FNsdecls[ADeclIndex].Uri;
end;

function TdxXmlNamespaceManager.LookupNamespaceDecl(const APrefix: string): Integer;
var
  ADeclIndex, AThisDecl: Integer;
begin
  if FUseHashTable then
  begin
    if FHashTable.TryGetValue(APrefix, ADeclIndex) then
    begin
      while (ADeclIndex <> -1) and (FNsdecls[ADeclIndex].Uri = '') do
        ADeclIndex := FNsdecls[ADeclIndex].PreviousNsIndex;
      Exit(ADeclIndex);
    end;
  end
  else
  begin
    AThisDecl := FLastDecl;
    while AThisDecl >= 0 do
    begin
      if (Pointer(FNsdecls[AThisDecl].Prefix) = Pointer(APrefix)) and (FNsdecls[AThisDecl].Uri <> '') then
        Exit(AThisDecl);
      Dec(AThisDecl);
    end;
    AThisDecl := FLastDecl;
    while AThisDecl >= 0 do
    begin
      if (FNsdecls[AThisDecl].Uri <> '') and (FNsdecls[AThisDecl].Prefix = APrefix) then
        Exit(AThisDecl);
      Dec(AThisDecl);
    end;
  end;
  Result := -1;
end;

function TdxXmlNamespaceManager.LookupPrefix(const AUri: string): string;
var
  AThisDecl: Integer;
begin
  AThisDecl := FLastDecl;
  while AThisDecl >= 0 do
  begin
    if FNsdecls[AThisDecl].Uri = AUri then
    begin
      Result := FNsdecls[AThisDecl].Prefix;
      if LookupNamespace(Result) = AUri then
        Exit;
    end;
    Dec(AThisDecl);
  end;
  Result := '';
end;

function TdxXmlNamespaceManager.HasNamespace(const APrefix: string): Boolean;
var
  AThisDecl: Integer;
begin
  AThisDecl := FLastDecl;
  while FNsdecls[AThisDecl].ScopeId = FScopeId do
  begin
    if (FNsdecls[AThisDecl].Uri <> '') and (FNsdecls[AThisDecl].Prefix = APrefix) then
    begin
      if (APrefix <> '') or (FNsdecls[AThisDecl].Uri <> '') then
        Exit(True)
      else
        Exit(False);
    end;
    Dec(AThisDecl);
  end;
  Result := False;
end;

function TdxXmlNamespaceManager.GetNamespaceDeclaration(AIdx: Integer; out APrefix: string; out AUri: string): Boolean;
begin
  AIdx := FLastDecl - AIdx;
  if AIdx < 0 then
  begin
    APrefix := '';
    AUri := '';
    Exit(False);
  end;

  APrefix := FNsdecls[AIdx].Prefix;
  AUri := FNsdecls[AIdx].Uri;

  Result := True;
end;

{ TdxXmlReader }

class function TdxXmlReader.CalcBufferSize(AInput: TStream): Integer;
var
  ABufferSize: Integer;
  ALen: Int64;
begin
  ABufferSize := DefaultBufferSize;
  ALen := AInput.Size;
  if ALen < ABufferSize then
    ABufferSize := Integer(ALen)
  else
    if ALen > MaxStreamLengthForDefaultBufferSize then
      ABufferSize := BiggerBufferSize;
  Result := ABufferSize;
end;

function TdxXmlReader.GetHasValue: Boolean;
begin
  Result := HasValueInternal(NodeType);
end;

function TdxXmlReader.GetName: string;
begin
  if Prefix = '' then
    Result := LocalName
  else
    Result := NameTable.Add(Prefix + ':' + LocalName);
end;

function TdxXmlReader.GetSettings: TdxXmlReaderSettings;
begin
  Result := nil;
end;

function TdxXmlReader.GetXmlSpace: TdxXmlSpace;
begin
  Result := TdxXmlSpace.None;
end;

function TdxXmlReader.ReadToFollowing(const ALocalName: string): Boolean;
var
  AName: string;
begin
  if ALocalName = '' then
    raise EInvalidArgument.Create(ALocalName);
  AName := NameTable.Add(ALocalName);
  while Read do
    if (NodeType = TdxXmlNodeType.Element) and (Pointer(ALocalName) = Pointer(Name)) then
      Exit(True);
  Result := False;
end;

function TdxXmlReader.ReadToFollowing(const ALocalName, ANameSpaceURI: string): Boolean;
var
  ALocalNameValue, ANamespaceURIValue: string;
begin
  if ALocalName = '' then
    raise EdxXmlArgumentNullException.Create('LocalName');
  if ANamespaceURI = '' then
    raise EdxXmlArgumentNullException.Create('namespaceURI');
  ALocalNameValue := NameTable.Add(ALocalName);
  ANamespaceURIValue := NameTable.Add(ANamespaceURI);
  while Read do
    if (NodeType = TdxXmlNodeType.Element) and (Pointer(ALocalNameValue) = Pointer(LocalName)) and
      (Pointer(ANamespaceURIValue) = Pointer(NamespaceURI)) then
        Exit(True);
  Result := False;
end;

procedure TdxXmlReader.Skip;
begin
  if ReadState <> TdxXmlReadState.Interactive then
    Exit;
  SkipSubtree;
end;

function TdxXmlReader.SkipSubtree: Boolean;
var
  ADepth: Integer;
begin
  MoveToElement;
  if (NodeType = TdxXmlNodeType.Element) and not IsEmptyElement then
  begin
    ADepth := Depth;

    while Read and (ADepth < Depth) do
    begin
    end;
    if NodeType = TdxXmlNodeType.EndElement then
      Exit(Read);
  end
  else
    Exit(Read);

  Result := False;
end;

class function TdxXmlReader.HasValueInternal(ANodeType: TdxXmlNodeType): Boolean;
begin
  Result := 0 <> (HasValueBitmap and (1 shl Ord(ANodeType)));
end;

{ TdxLineInfo }

constructor TdxLineInfo.Create(ALineNo: Integer; ALinePos: Integer);
begin
  FLineNo := ALineNo;
  FLinePos := ALinePos;
end;

procedure TdxLineInfo.&Set(ALineNo: Integer; ALinePos: Integer);
begin
  FLineNo := ALineNo;
  FLinePos := ALinePos;
end;

{ TdxNodeData }

constructor TdxNodeData.Create;
begin
  FValueLocation := TValueLocation.CharsBuffer;
  Clear(TdxXmlNodeType.None);
  FXmlContextPushed := False;
end;

class function TdxNodeData.GetNone: TdxNodeData;
begin
  if FNone = nil then
    FNone := TdxNodeData.Create;
  Result := FNone;
end;

function TdxNodeData.GetLineNo: Integer;
begin
  Result := FLineInfo.LineNo;
end;

function TdxNodeData.GetLinePos: Integer;
begin
  Result := FLineInfo.LinePos;
end;

function TdxNodeData.GetIsEmptyElement: Boolean;
begin
  Result := (FType = TdxXmlNodeType.Element) and FIsEmptyOrDefault;
end;

procedure TdxNodeData.SetIsEmptyElement(const AValue: Boolean);
begin
  Assert(FType = TdxXmlNodeType.Element);
  FIsEmptyOrDefault := AValue;
end;

function TdxNodeData.GetIsDefaultAttribute: Boolean;
begin
  Result := (FType = TdxXmlNodeType.Attribute) and FIsEmptyOrDefault;
end;

procedure TdxNodeData.SetIsDefaultAttribute(const AValue: Boolean);
begin
  Assert(FType = TdxXmlNodeType.Attribute);
  FIsEmptyOrDefault := AValue;
end;

function TdxNodeData.GetStringValue: string;
begin
  Assert((FValueStartPos >= 0) or (FValueLocation = TValueLocation.ValueString), 'Value not ready.');
  if ValueBuffered then
    SetString(FValue, PChar(@FChars[FValueStartPos]), FValueLength);
  Result := FValue;
end;

function TdxNodeData.GetValueBuffered: Boolean;
begin
  Result := FValueLocation = TValueLocation.CharsBuffer;
end;

procedure TdxNodeData.Clear(AType: TdxXmlNodeType);
begin
  FType := AType;
  ClearName;
  FValue := '';
  FValueLocation := TValueLocation.ValueString;
  FValueStartPos := -1;
  FNameWPrefix := '';
end;

procedure TdxNodeData.ClearName;
begin
  FLocalName := '';
  FPrefix := '';
  FNamespace := '';
  FNameWPrefix := '';
end;

procedure TdxNodeData.SetLineInfo(ALineNo, ALinePos: Integer);
begin
  FLineInfo.&Set(ALineNo, ALinePos);
end;

procedure TdxNodeData.SetLineInfo2(ALineNo, ALinePos: Integer);
begin
  FLineInfo2.&Set(ALineNo, ALinePos);
end;

procedure TdxNodeData.SetValueNode(AType: TdxXmlNodeType; const AValue: string);
begin
  FType := AType;
  ClearName;
  FValue := AValue;
  FValueLocation := TValueLocation.ValueString;
  FValueStartPos := -1;
end;

procedure TdxNodeData.SetValueNode(AType: TdxXmlNodeType; const AChars: TCharArray; AStartPos, ALength: Integer);
begin
  FType := AType;
  ClearName;

  FValue := '';
  FValueLocation := TValueLocation.CharsBuffer;
  FChars := AChars;
  FValueStartPos := AStartPos;
  FValueLength := ALength;
end;

procedure TdxNodeData.SetNamedNode(AType: TdxXmlNodeType; const ALocalName: string);
begin
  SetNamedNode(AType, ALocalName, '', ALocalName);
end;

procedure TdxNodeData.SetNamedNode(AType: TdxXmlNodeType; const ALocalName, APrefix, ANameWPrefix: string);
begin
  Assert(Length(ALocalName) > 0);

  FType := AType;
  FLocalName := ALocalName;
  FPrefix := APrefix;
  FNameWPrefix := ANameWPrefix;
  FNamespace := '';
  FValue := '';
  FValueLocation := TValueLocation.ValueString;
  FValueStartPos := -1;
end;

procedure TdxNodeData.SetValue(const AValue: string);
begin
  FValueStartPos := -1;
  FValue := AValue;
  FValueLocation := TValueLocation.ValueString;
end;

procedure TdxNodeData.SetValue(const AChars: TCharArray; AStartPos: Integer; ALength: Integer);
begin
  FValue := '';
  FValueLocation := TValueLocation.CharsBuffer;
  FChars := AChars;
  FValueStartPos := AStartPos;
  FValueLength := ALength;
end;

procedure TdxNodeData.OnBufferInvalidated;
begin
  if FValueLocation = TValueLocation.CharsBuffer then
  begin
    Assert(FChars <> nil);
    SetString(FValue, PChar(@FChars[FValueStartPos]), FValueLength);
    FValueLocation := TValueLocation.ValueString;
  end;
  FValueStartPos := -1;
end;

function TdxNodeData.GetNameWPrefix(ANameTable: TdxXmlNameTable): string;
begin
  if FNameWPrefix <> '' then
    Result := FNameWPrefix
  else
    Result := CreateNameWPrefix(ANameTable);
end;

function TdxNodeData.CreateNameWPrefix(AXmlNameTable: TdxXmlNameTable): string;
begin
  Assert(FNameWPrefix = '');
  if FPrefix = '' then
    FNameWPrefix := FLocalName
  else
    FNameWPrefix := AXmlNameTable.Add(Concat(FPrefix, ':', FLocalName));
  Result := FNameWPrefix;
end;

function TdxNodeData.CompareTo(AObject: TObject): Integer;
var
  AOther: TdxNodeData;
begin
  AOther := Safe<TdxNodeData>.Cast(AObject);
  if AOther <> nil then
  begin
    if FLocalName = AOther.FLocalName then
      if FNamespace = AOther.FNamespace then
        Result := 0
      else
        Result := CompareStr(FNamespace, AOther.FNamespace)
    else
      Result := CompareStr(FLocalName, AOther.FLocalName);
  end
  else
  begin
    Assert(False, 'We should never get to this point.');
    Result := 1;
  end;
end;

procedure TdxNodeData.CopyTo(AValueOffset: Integer; ASb: TStringBuilder);
begin
  if FValue = '' then
  begin
    Assert(FValueStartPos <> -1);
    Assert(FChars <> nil);
    ASb.Append(FChars, FValueStartPos + AValueOffset, FValueLength - AValueOffset);
  end
  else
    if AValueOffset <= 0 then
      ASb.Append(FValue)
    else
      ASb.Append(FValue, AValueOffset, Length(FValue) - AValueOffset);
end;

{ TdxXmlTextReaderImpl }

constructor TdxXmlTextReaderImpl.Create;
begin
  inherited Create;
  FCurrentAttributeIndex := -1;
  FSupportNamespaces := True;
  FFragmentType := TdxXmlNodeType.Document;
end;

class procedure TdxXmlTextReaderImpl.BlockCopyChars(ASource: TCharArray; ASourceOffset: Integer;
  ADestination: TCharArray; ADestinationOffset, ACount: Integer);
begin
  Move(ASource[ASourceOffset], ADestination[ADestinationOffset], ACount * SizeOf(Char));
end;

class function TdxXmlTextReaderImpl.StrEqual(const AChars: TCharArray; AStrPos1, AStrLen1: Integer;
  const AStr2: string): Boolean;
var
  I: Integer;
begin
  if AStrLen1 <> Length(AStr2) then
    Exit(False);
  I := 0;
  while (I < AStrLen1) and (AChars[AStrPos1 + I] = AStr2[I + 1]) do
    Inc(I);
  Result := I = AStrLen1;
end;

class function TdxXmlTextReaderImpl.StripSpaces(const AValue: string): string;
var
  ALen: Integer;
  ADest, ASrc, ASrcEnd, ADestStart: PChar;
begin
  ALen := Length(AValue);
  if ALen = 0 then
    Exit('');
  ASrc := PChar(AValue);
  ASrcEnd := ASrc + ALen;
  while ASrc^ = #$0020 do
  begin
    Inc(ASrc);
    if ASrc = ASrcEnd then
      Exit(' ');
  end;
  SetLength(Result, ALen);
  ADestStart := PChar(Result);
  ADest := ADestStart;
  while ASrcEnd > ASrc do
  begin
    if ASrc^ = #$0020 then
    begin
      while (ASrcEnd > ASrc) and (ASrc^ = #$0020) do
        Inc(ASrc);

      if ASrcEnd = ASrc then
      begin
        SetLength(Result, ADest - ADestStart);
        Exit;
      end;
      ADest^ := #$0020;
      Inc(ADest);
    end;
    ADest^ := ASrc^;
    Inc(ADest);
    Inc(ASrc);
  end;
  SetLength(Result, ADest - ADestStart);
end;

class procedure TdxXmlTextReaderImpl.StripSpaces(var AValue: TCharArray; AIndex: Integer; var ALen: Integer);
var
  AStartPos, AEndPos, AOffset, I, J: Integer;
  ACh: Char;
begin
  if ALen <= 0 then
    Exit;
  AStartPos := AIndex;
  AEndPos := AIndex + ALen;
  while AValue[AStartPos] = #$0020 do
  begin
    Inc(AStartPos);
    if AStartPos = AEndPos then
    begin
      ALen := 1;
      Exit;
    end;
  end;
  AOffset := AStartPos - AIndex;
  I := AStartPos;
  while I < AEndPos do
  begin
    ACh := AValue[I];
    if ACh = #$0020 then
    begin
      J := I + 1;
      while (J < AEndPos) and (AValue[J] = #$0020) do
        Inc(J);
      if J = AEndPos then
      begin
        Inc(AOffset, J - I);
        Break;
      end;
      if J > I + 1 then
      begin
        Inc(AOffset, J - I - 1);
        I := J - 1;
      end;
    end;
    AValue[I - AOffset] := ACh;
    Inc(I);
  end;
  Dec(ALen, AOffset);
end;

function TdxXmlTextReaderImpl.CheckEncoding(const ANewEncodingName: string): TEncoding;
begin
  Assert(SameText(ANewEncodingName, 'UTF-8'), 'UTF-8');
  Result := TEncoding.UTF8;
end;

procedure TdxXmlTextReaderImpl.ClearNodes;
var
  I: Integer;
begin
  for I := 0 to Length(FNodes) - 1 do
    FNodes[I].Free;
end;

constructor TdxXmlTextReaderImpl.Create(AStream: TStream; const ABytes: TBytes; AByteCount: Integer;
  ASettings: TdxXmlReaderSettings; ACloseInput: Boolean);
begin
  Create(ASettings);

  FCloseInput := ACloseInput;

  FLaterInitParam := TLaterInitParam.Create;
  FLaterInitParam.InputStream := AStream;
  FLaterInitParam.InputBytes := ABytes;
  FLaterInitParam.InputByteCount := AByteCount;

  FinishInitStream;
end;

destructor TdxXmlTextReaderImpl.Destroy;
begin
  FreeAndNil(FInternalNameTable);
  FNamespaceManager.Free;
  FreeAndNil(FXmlContext);
  FreeAndNil(FStringBuilder);
  ClearNodes;
  inherited Destroy;
end;

function TdxXmlTextReaderImpl.DetectEncoding: TEncoding;
var
  AFirst2Bytes, ANext2Bytes: Integer;
begin
  Assert(FParsingState.Bytes <> nil);
  Assert(FParsingState.BytePos = 0);

  if FParsingState.BytesUsed < 2 then
    Exit(nil);
  AFirst2Bytes := (FParsingState.Bytes[0] shl 8) or (FParsingState.Bytes[1]);
  if (FParsingState.BytesUsed >= 4) then
    ANext2Bytes := ((FParsingState.Bytes[2] shl 8) or (FParsingState.Bytes[3]))
  else
    ANext2Bytes := 0;

  case AFirst2Bytes of
    $0000:
      case ANext2Bytes of
        $FEFF, $003C, $FFFE, $3C00:
          Exit(NotImplemented);
      end;
    $FEFF:
      if ANext2Bytes = $0000 then
        Exit(NotImplemented)
      else
        Exit(NotImplemented);
    $FFFE:
      if ANext2Bytes = $0000 then
        Exit(NotImplemented)
      else
        Exit(TEncoding.Unicode);
    $3C00:
      if ANext2Bytes = $0000 then
        Exit(NotImplemented)
      else
        Exit(TEncoding.Unicode);
    $003C:
      if ANext2Bytes = $0000 then
        Exit(NotImplemented)
      else
        Exit(TEncoding.BigEndianUnicode);
    $4C6F:
      if ANext2Bytes = $A794 then
        Throw('UnknownEncoding: ebcdic');
    $EFBB:
      if (ANext2Bytes and $FF00) = $BF00 then
        Exit(TEncoding.UTF8);
  end;
  Result := nil;
end;

constructor TdxXmlTextReaderImpl.Create(ASettings: TdxXmlReaderSettings);
var
  ANameTable: TdxXmlNameTable;
begin
  Create;

  FXmlContext := TXmlContext.Create;
  FStringBuilder := TStringBuilder.Create;

  ANameTable := ASettings.NameTable;
  if ANameTable = nil then
  begin
    FInternalNameTable := TdxXmlNameTable.Create;
    ANameTable := FInternalNameTable;
    Assert(FNameTableFromSettings = False);
  end
  else
    FNameTableFromSettings := True;
  FNameTable := ANameTable;
  FNamespaceManager := TdxXmlNamespaceManager.Create(ANameTable);

  ANameTable.Add('');
  FXml := ANameTable.Add('xml');
  FXmlNs := ANameTable.Add('xmlns');

  Assert(FIndex = 0);

  SetLength(FNodes, NodesInitialSize);
  FNodes[0] := TdxNodeData.Create;
  FCurrentNode := FNodes[0];

  if (ASettings.IgnoreWhitespace) then
    FWhitespaceHandling := TdxWhitespaceHandling.Significant
  else
    FWhitespaceHandling := TdxWhitespaceHandling.All;
  FNormalize := True;
  FIgnorePIs := ASettings.IgnoreProcessingInstructions;
  FIgnoreComments := ASettings.IgnoreComments;
  FCheckCharacters := ASettings.CheckCharacters;
  FLineNumberOffset := ASettings.LineNumberOffset;
  FLinePositionOffset := ASettings.LinePositionOffset;
  FParsingState.LineNo := FLineNumberOffset + 1;
  FParsingState.LineStartPos := -FLinePositionOffset - 1;
  FCurrentNode.SetLineInfo(FParsingState.LineNo - 1, FParsingState.LinePos - 1);
  FMaxCharactersInDocument := ASettings.MaxCharactersInDocument;

  FCharactersInDocument := 0;
  FCharactersFromEntities := 0;

  FParsingFunction := TParsingFunction.SwitchToInteractiveXmlDecl;
  FNextParsingFunction := TParsingFunction.DocumentContent;

  case ASettings.ConformanceLevel of
    TdxXMLConformanceLevel.Auto:
      begin
        FFragmentType := TdxXmlNodeType.None;
        FFragment := True;
      end;
    TdxXMLConformanceLevel.Fragment:
      begin
        FFragmentType := TdxXmlNodeType.Element;
        FFragment := True;
      end;
    TdxXMLConformanceLevel.Document:
      FFragmentType := TdxXmlNodeType.Document;
    else
    begin
      Assert(False);
      FFragmentType := TdxXmlNodeType.Document;
    end;
  end;
end;

procedure TdxXmlTextReaderImpl.FinishInit;
begin
  FinishInitStream;
end;

procedure TdxXmlTextReaderImpl.FinishInitStream;
begin
  InitStreamInput(FLaterInitParam.InputStream, FLaterInitParam.InputBytes, FLaterInitParam.InputByteCount, nil);
  FreeAndNil(FLaterInitParam);
end;

procedure TdxXmlTextReaderImpl.FinishPartialValue;
var
  AStartPos, AEndPos, AOrChars: Integer;
begin
  Assert(FStringBuilder.Length = 0);
  Assert(FParsingFunction = TParsingFunction.PartialTextValue);

  FCurrentNode.CopyTo(FReadValueOffset, FStringBuilder);

  AOrChars := 0;
  while not ParseText(AStartPos, AEndPos, AOrChars) do
    FStringBuilder.Append(FParsingState.Chars, AStartPos, AEndPos - AStartPos);
  FStringBuilder.Append(FParsingState.Chars, AStartPos, AEndPos - AStartPos);

  Assert(FStringBuilder.Length > 0);
  FCurrentNode.SetValue(FStringBuilder.ToString);
  FStringBuilder.Length := 0;
end;

procedure TdxXmlTextReaderImpl.FullAttributeCleanup;
var
  I: Integer;
  AAttribute: TdxNodeData;
begin
  for I := FIndex + 1 to FIndex + FAttributeCount + 1 - 1 do
  begin
    AAttribute := FNodes[I];
    AAttribute.IsDefaultAttribute := False;
  end;
  FFullAttributeCleanup := False;
end;

function TdxXmlTextReaderImpl.GetAttribute(const AName: string): string;
var
  I: Integer;
begin
  if TdxStringHelper.IndexOf(AName, ':') = -1 then
    I := GetIndexOfAttributeWithoutPrefix(AName)
  else
    I := GetIndexOfAttributeWithPrefix(AName);
  if I >= 0 then
    Result := FNodes[I].StringValue
  else
    Result := '';
end;

function TdxXmlTextReaderImpl.GetAttribute(const AAttribute, ANamespaceURI: string): string;
var
  ALocalName, ANamespace: String;
  I: Integer;
begin
  ANamespace := FNameTable.Get(ANamespaceURI);
  ALocalName := FNameTable.Get(AAttribute);
  for I := FIndex + 1 to FIndex + FAttributeCount + 1 - 1 do
    if (FNodes[I].LocalName = ALocalName) and (FNodes[I].Namespace = ANamespace) then
      Exit(FNodes[I].StringValue);
  Result := '';
end;

function TdxXmlTextReaderImpl.GetChars(AMaxCharsCount: Integer): Integer;
var
  ABytesCount, ACharsCount: Integer;
begin
  Assert((FParsingState.Stream <> nil) and (FParsingState.Decoder <> nil) and (FParsingState.Bytes <> nil));
  Assert(AMaxCharsCount <= Length(FParsingState.Chars) - FParsingState.CharsUsed - 1);

  ABytesCount := FParsingState.BytesUsed - FParsingState.BytePos;
  if ABytesCount = 0 then
    Exit(0);
  try
    FParsingState.Decoder.Convert(FParsingState.Bytes, FParsingState.BytePos, ABytesCount,
      FParsingState.Chars, FParsingState.CharsUsed, AMaxCharsCount,
      ABytesCount, ACharsCount);
  except
    InvalidCharRecovery(ABytesCount, ACharsCount);
  end;
  Inc(FParsingState.BytePos, ABytesCount);
  Inc(FParsingState.CharsUsed, ACharsCount);
  Assert(AMaxCharsCount >= ACharsCount);
  Result := ACharsCount;
end;

function TdxXmlTextReaderImpl.GetDepth: Integer;
begin
  Result := FCurrentNode.Depth;
end;

function TdxXmlTextReaderImpl.GetInAttributeValueIterator: Boolean;
begin
  Result := (FAttributeCount > 0) and (FParsingFunction >= TParsingFunction.InReadAttributeValue);
end;

function TdxXmlTextReaderImpl.GetIndexOfAttributeWithoutPrefix(const AName: string): Integer;
var
  I: Integer;
  ANameValue: string;
begin
  ANameValue := FNameTable.Get(AName);
  if ANameValue = '' then
    Exit(-1);
  for I := FIndex + 1 to FIndex + FAttributeCount + 1 - 1 do
    if (FNodes[I].LocalName = ANameValue) and (Length(FNodes[I].Prefix) = 0) then
      Exit(I);
  Result := -1;
end;

function TdxXmlTextReaderImpl.GetIndexOfAttributeWithPrefix(const AName: string): Integer;
var
  I: Integer;
  ANameValue: string;
begin
  ANameValue := FNameTable.Add(AName);
  if ANameValue = '' then
    Exit(-1);
  for I := FIndex + 1 to FIndex + FAttributeCount do
    if (FNodes[I].GetNameWPrefix(FNameTable) = ANameValue) then
      Exit(I);
  Result := -1;
end;

function TdxXmlTextReaderImpl.GetLocalName: string;
begin
  Result := FCurrentNode.LocalName;
end;

function TdxXmlTextReaderImpl.GetNamespaceURI: string;
begin
  Result := FCurrentNode.Namespace;
end;

function TdxXmlTextReaderImpl.GetNameTable: TdxXmlNameTable;
begin
  Result := FNameTable;
end;

function TdxXmlTextReaderImpl.GetNodeType: TdxXmlNodeType;
begin
  Result := FCurrentNode.&Type;
end;

function TdxXmlTextReaderImpl.GetPrefix: string;
begin
  Result := FCurrentNode.Prefix;
end;

procedure TdxXmlTextReaderImpl.InitStreamInput(AStream: TStream;
  const ABytes: TBytes; AByteCount: Integer; AEncoding: TEncoding);
var
  ABufferSize, ARead, APreambleLen, I: Integer;
  APreamble: TBytes;
begin
  FParsingState.Stream := AStream;
  if ABytes <> nil then
  begin
    FParsingState.Bytes := ABytes;
    FParsingState.BytesUsed := AByteCount;
    ABufferSize := Length(FParsingState.Bytes);
  end
  else
  begin
    ABufferSize := TdxXmlReader.CalcBufferSize(AStream);

    if (FParsingState.Bytes = nil) or (Length(FParsingState.Bytes) < ABufferSize) then
      SetLength(FParsingState.Bytes, ABufferSize);
  end;

  if (FParsingState.Chars = nil) or (Length(FParsingState.Chars) < ABufferSize + 1) then
    SetLength(FParsingState.Chars, ABufferSize + 1);

  FParsingState.BytePos := 0;
  while (FParsingState.BytesUsed < 4) and (Length(FParsingState.Bytes) - FParsingState.BytesUsed > 0) do
  begin
    ARead := AStream.Read(FParsingState.Bytes[FParsingState.BytesUsed], Length(FParsingState.Bytes) - FParsingState.BytesUsed);
    if ARead = 0 then
    begin
      FParsingState.IsStreamEof := True;
      Break;
    end;
    FParsingState.BytesUsed := FParsingState.BytesUsed + ARead;
  end;
  if AEncoding = nil then
    AEncoding := DetectEncoding;
  SetupEncoding(AEncoding);
  APreamble := FParsingState.Encoding.GetPreamble;
  APreambleLen := Length(APreamble);
  I := 0;
  while (I < APreambleLen) and (I < FParsingState.BytesUsed) do
  begin
    if FParsingState.Bytes[I] <> APreamble[I] then
      Break;
    Inc(I);
  end;
  if I = APreambleLen then
    FParsingState.BytePos := APreambleLen;

  FDocumentStartBytePos := FParsingState.BytePos;

  FParsingState.EolNormalized := not FNormalize;

  FParsingState.AppendMode := True;
  ReadData;
end;

procedure TdxXmlTextReaderImpl.InvalidCharRecovery(var ABytesCount: Integer; out ACharsCount: Integer);
var
  ACharsDecoded, ABytesDecoded, AChDec, ABDec: Integer;
begin
  ACharsDecoded := 0;
  ABytesDecoded := 0;
  try
    while ABytesDecoded < ABytesCount do
    begin
      FParsingState.Decoder.Convert(FParsingState.Bytes, FParsingState.BytePos + ABytesDecoded, 1, FParsingState.Chars, FParsingState.CharsUsed + ACharsDecoded, 1,
        ABDec, AChDec);
      Inc(ACharsDecoded, AChDec);
      Inc(ABytesDecoded, ABDec);
    end;
    Assert(False, 'We should get an exception again.');
  except
  end;

  if ACharsDecoded = 0 then
    Throw(FParsingState.CharsUsed, SXmlInvalidCharInThisEncoding);
  ACharsCount := ACharsDecoded;
  ABytesCount := ABytesDecoded;
end;

function TdxXmlTextReaderImpl.IsEmptyElement: Boolean;
begin
  Result := FCurrentNode.IsEmptyElement;
end;

function TdxXmlTextReaderImpl.LookupNamespace(const APrefix: string): string;
begin
  if not FSupportNamespaces then
    Result := ''
  else
    Result := FNamespaceManager.LookupNamespace(APrefix);
end;

function TdxXmlTextReaderImpl.MoveToElement: Boolean;
begin
  if FCurrentNode.&Type <> TdxXmlNodeType.Attribute then
    Exit(False);
  FCurrentAttributeIndex := -1;
  FCurrentNode := FNodes[FIndex];

  Result := True;
end;

function TdxXmlTextReaderImpl.MoveToNextAttribute: Boolean;
begin
  if FCurrentAttributeIndex + 1 < FAttributeCount then
  begin
    Inc(FCurrentAttributeIndex);
    FCurrentNode := FNodes[FIndex + 1 + FCurrentAttributeIndex];
    Result := True;
  end
  else
    Result := False;
end;

procedure TdxXmlTextReaderImpl.OnEof;
begin
  Assert(FParsingState.IsEof);
  FCurrentNode := FNodes[0];
  FCurrentNode.Clear(TdxXmlNodeType.None);
  FCurrentNode.SetLineInfo(FParsingState.LineNo, FParsingState.LinePos);

  FParsingFunction := TParsingFunction.Eof;
  FReadState := TdxXmlReadState.EndOfFile;
end;

procedure TdxXmlTextReaderImpl.ParseCData;
begin
  ParseCDataOrComment(TdxXmlNodeType.CDATA);
end;

procedure TdxXmlTextReaderImpl.ParseCDataOrComment(AType: TdxXmlNodeType);
var
  AStartPos, AEndPos: Integer;
begin
  if FParsingMode = TParsingMode.Full then
  begin
    FCurrentNode.SetLineInfo(FParsingState.LineNo, FParsingState.LinePos);
    Assert(FStringBuilder.Length = 0);
    if ParseCDataOrComment(AType, AStartPos, AEndPos) then
      FCurrentNode.SetValueNode(AType, FParsingState.Chars, AStartPos, AEndPos - AStartPos)
    else
    begin
      repeat
        FStringBuilder.Append(FParsingState.Chars, AStartPos, AEndPos - AStartPos);
      until ParseCDataOrComment(AType, AStartPos, AEndPos);
      FStringBuilder.Append(FParsingState.Chars, AStartPos, AEndPos - AStartPos);
      FCurrentNode.SetValueNode(AType, FStringBuilder.ToString);
      FStringBuilder.Length := 0;
    end;
  end
  else
    while not ParseCDataOrComment(AType, AStartPos, AEndPos) do ;
end;

function TdxXmlTextReaderImpl.ParseCDataOrComment(AType: TdxXmlNodeType; out AOutStartPosition, AOutEndPosition: Integer): Boolean;
label
  ReturnPartial;
var
  APosition, ARcount, ARpos: Integer;
  AChars: TCharArray;
  AStopChar, ACh: Char;
begin
  if FParsingState.CharsUsed - FParsingState.CharPos < 3 then
  begin
    if ReadData = 0 then
      Throw(SXmlUnexpectedEOF, IfThen(AType = TdxXmlNodeType.Comment, 'Comment', 'CDATA'));
  end;

  APosition := FParsingState.CharPos;
  AChars := FParsingState.Chars;
  ARcount := 0;
  ARpos := -1;
  if AType = TdxXmlNodeType.Comment then
    AStopChar := '-'
  else
    AStopChar := ']';

  while True do
  begin
    ACh := AChars[APosition];
    while (ACh <> AStopChar) and ((TdxXmlCharType.CharProperties[ACh] and TdxXmlCharType.Text) <> 0) do
    begin
      Inc(APosition);
      ACh := AChars[APosition];
    end;

    if AChars[APosition] = AStopChar then
    begin
      if AChars[APosition + 1] = AStopChar then
      begin
        if AChars[APosition + 2] = '>' then
        begin
          if ARcount > 0 then
          begin
            Assert(not FParsingState.EolNormalized);
            ShiftBuffer(ARpos + ARcount, ARpos, APosition - ARpos - ARcount);
            AOutEndPosition := APosition - ARcount;
          end
          else
            AOutEndPosition := APosition;
          AOutStartPosition := FParsingState.CharPos;
          FParsingState.CharPos := APosition + 3;
          Exit(True);
        end
        else
          if APosition + 2 = FParsingState.CharsUsed then
            goto ReturnPartial
          else
            if AType = TdxXmlNodeType.Comment then
              Throw(APosition, SXmlInvalidCommentChars);
      end
      else
        if APosition + 1 = FParsingState.CharsUsed then
          goto ReturnPartial;
      Inc(APosition);
      Continue;
    end
    else
    begin
      case AChars[APosition] of
        #$000A:
          begin
            Inc(APosition);
            OnNewLine(APosition);
            Continue;
          end;
        #$000D:
          begin
            if AChars[APosition + 1] = #$000A then
            begin
              if not FParsingState.EolNormalized and (FParsingMode = TParsingMode.Full) then
              begin
                if APosition - FParsingState.CharPos > 0 then
                begin
                  if ARcount = 0 then
                  begin
                    ARcount := 1;
                    ARpos := APosition;
                  end
                  else
                  begin
                    ShiftBuffer(ARpos + ARcount, ARpos, APosition - ARpos - ARcount);
                    ARpos := APosition - ARcount;
                    Inc(ARcount);
                  end;
                end
                else
                  Inc(FParsingState.CharPos);
              end;
              Inc(APosition, 2);
            end
            else
              if (APosition + 1 < FParsingState.CharsUsed) or FParsingState.IsEof then
              begin
                if not FParsingState.EolNormalized then
                  AChars[APosition] := #$000A;
                Inc(APosition);
              end
              else
                goto ReturnPartial;
            OnNewLine(APosition);
            Continue;
          end;
        '<', '&', ']', #$0009:
          begin
            Inc(APosition);
            Continue;
          end
        else
        begin
          if APosition = FParsingState.CharsUsed then
            goto ReturnPartial;
          ACh := AChars[APosition];
          if TdxXmlCharType.IsHighSurrogate(ACh) then
          begin
            if APosition + 1 = FParsingState.CharsUsed then
              goto ReturnPartial;
            Inc(APosition);
            if TdxXmlCharType.IsLowSurrogate(AChars[APosition]) then
            begin
              Inc(APosition);
              Continue;
            end;
          end;
          ThrowInvalidChar(AChars, FParsingState.CharsUsed, APosition);
        end;
      end;
    end;

ReturnPartial:
    if ARcount > 0 then
    begin
      ShiftBuffer(ARpos + ARcount, ARpos, APosition - ARpos - ARcount);
      AOutEndPosition := APosition - ARcount;
    end
    else
      AOutEndPosition := APosition;
    AOutStartPosition := FParsingState.CharPos;

    FParsingState.CharPos := APosition;
    Exit(False);
  end;
end;

function TdxXmlTextReaderImpl.ParseComment: Boolean;
var
  AOldParsingMode: TParsingMode;
begin
  if FIgnoreComments then
  begin
    AOldParsingMode := FParsingMode;
    FParsingMode := TParsingMode.SkipNode;
    ParseCDataOrComment(TdxXmlNodeType.Comment);
    FParsingMode := AOldParsingMode;
    Result := False;
  end
  else
  begin
    ParseCDataOrComment(TdxXmlNodeType.Comment);
    Result := True;
  end;
end;

function TdxXmlTextReaderImpl.ParseDocumentContent: Boolean;
label
  LblReadData;
var
  AMangoQuirks, ANeedMoreChars: Boolean;
  APosition: Integer;
  AChars: TCharArray;
begin
  AMangoQuirks := False;
  while True do
  begin
    ANeedMoreChars := False;
    APosition := FParsingState.CharPos;
    AChars := FParsingState.Chars;

    if AChars[APosition] = '<' then
    begin
      ANeedMoreChars := True;
      if FParsingState.CharsUsed - APosition < 4 then
        goto LblReadData;
      Inc(APosition);
      case AChars[APosition] of
        '?':
          begin
            FParsingState.CharPos := APosition + 1;
            if ParsePI then
              Exit(True);
            Continue;
          end;
        '!':
          begin
            Inc(APosition);
            if FParsingState.CharsUsed - APosition < 2 then
              goto LblReadData;

            if AChars[APosition] = '-' then
            begin
              if AChars[APosition + 1] = '-' then
              begin
                FParsingState.CharPos := APosition + 2;
                if ParseComment then
                  Exit(True);
                Continue;
              end
              else
                ThrowUnexpectedToken(APosition + 1, '-');
            end
            else
              if AChars[APosition] = '[' then
              begin
                if FFragmentType <> TdxXmlNodeType.Document then
                begin
                  Inc(APosition);
                  if FParsingState.CharsUsed - APosition < 6 then
                    goto LblReadData;
                  if StrEqual(AChars, APosition, 6, 'CDATA[') then
                  begin
                    FParsingState.CharPos := APosition + 6;
                    ParseCData;
                    if FFragmentType = TdxXmlNodeType.None then
                      FFragmentType := TdxXmlNodeType.Element;
                    Exit(True);
                  end
                  else
                    ThrowUnexpectedToken(APosition, 'CDATA[');
                end
                else
                  Throw(FParsingState.CharPos, 'InvalidRootData');
              end
              else
                Throw(SDTDNotImplemented);
          end;
        '/':
          Throw(APosition + 1, SXmlUnexpectedEndTag);
        else
          begin
            if FRootElementParsed then
            begin
              if FFragmentType = TdxXmlNodeType.Document then
                Throw(APosition, SXmlMultipleRoots);
              if FFragmentType = TdxXmlNodeType.None then
                FFragmentType := TdxXmlNodeType.Element;
            end;
            FParsingState.CharPos := APosition;
            FRootElementParsed := True;
            ParseElement;
            Exit(True);
          end;
      end;
    end
    else
      if AChars[APosition] = '&' then
        Throw(SDTDNotImplemented)
      else
        if (APosition = FParsingState.CharsUsed) or (AMangoQuirks and (AChars[APosition] = #0)) then
          goto LblReadData
        else
        begin
          if FFragmentType = TdxXmlNodeType.Document then
          begin
            if ParseRootLevelWhitespace then
              Exit(True);
          end
          else
          begin
            if ParseText then
            begin
              if (FFragmentType = TdxXmlNodeType.None) and (FCurrentNode.&Type = TdxXmlNodeType.Text) then
                FFragmentType := TdxXmlNodeType.Element;
              Exit(True);
            end;
          end;
          Continue;
        end;

    Assert((APosition = FParsingState.CharsUsed) and not FParsingState.IsEof);

LblReadData:
    if ReadData = 0 then
    begin
      if ANeedMoreChars then
        Throw(SXmlInvalidRootData);

      Assert(FIndex = 0);

      if not FRootElementParsed and (FFragmentType = TdxXmlNodeType.Document) then
        ThrowWithoutLineInfo(SXmlMissingRoot);

      if FFragmentType = TdxXmlNodeType.None then
        if FRootElementParsed then
          FFragmentType := TdxXmlNodeType.Document
        else
          FFragmentType := TdxXmlNodeType.Element;
      OnEof;
      Exit(False);
    end;

    AChars := FParsingState.Chars;
  end;
end;

function TdxXmlTextReaderImpl.AddNode(ANodeIndex, ANodeDepth: Integer): TdxNodeData;
begin
  Assert(ANodeIndex < Length(FNodes));
  Assert(FNodes[Length(FNodes) - 1] = nil);

  Result := FNodes[ANodeIndex];
  if Result = nil then
    Result := AllocNode(ANodeIndex, ANodeDepth)
  else
    Result.Depth := ANodeDepth;
end;

function TdxXmlTextReaderImpl.AllocNode(ANodeIndex, ANodeDepth: Integer): TdxNodeData;
begin
  Assert(ANodeIndex < Length(FNodes));
  if ANodeIndex >= Length(FNodes) - 1 then
    SetLength(FNodes, Length(FNodes) * 2);

  Assert(ANodeIndex < Length(FNodes));

  Result := FNodes[ANodeIndex];
  if Result = nil then
  begin
    Result := TdxNodeData.Create;
    FNodes[ANodeIndex] := Result;
  end;
  Result.Depth := ANodeDepth;
end;

procedure TdxXmlTextReaderImpl.AttributeNamespaceLookup;
var
  I: Integer;
  AAt: TdxNodeData;
begin
  for I := FIndex + 1 to FIndex + FAttributeCount do
  begin
    AAt := FNodes[I];
    if (AAt.&Type = TdxXmlNodeType.Attribute) and (Length(AAt.Prefix) > 0) then
      AAt.Namespace := LookupNamespace(AAt);
  end;
end;

function TdxXmlTextReaderImpl.AddAttribute(const ALocalName, APrefix: string; const ANameWPrefix: string): TdxNodeData;
var
  ANewAttr, AAttr: TdxNodeData;
  AAttrHash, I: Integer;
begin
  ANewAttr := AddNode(FIndex + FAttributeCount + 1, FIndex + 1);
  ANewAttr.SetNamedNode(TdxXmlNodeType.Attribute, ALocalName, APrefix, ANameWPrefix);
  AAttrHash := 1 shl (Ord(PChar(ALocalName)^) and $001F);
  if (FAttributeHashTable and AAttrHash) = 0 then
    FAttributeHashTable := FAttributeHashTable or AAttrHash
  else
  begin
    if FAttributeDuplicateWalkCount < MaxAttrDuplWalkCount then
    begin
      Inc(FAttributeDuplicateWalkCount);
      for I := FIndex + 1 to FIndex + FAttributeCount + 1 - 1 do
      begin
        AAttr := FNodes[I];
        Assert(AAttr.&Type = TdxXmlNodeType.Attribute);
        if AAttr.LocalName = ANewAttr.LocalName then
        begin
          FAttributeDuplicateWalkCount := MaxAttrDuplWalkCount;
          Break;
        end;
      end;
    end;
  end;

  Inc(FAttributeCount);
  Result := ANewAttr;
end;

function TdxXmlTextReaderImpl.AddAttribute(AEndNamePos, AColonPos: Integer): TdxNodeData;
var
  ALocalName, APrefix: string;
  AStartPos, APrefixLen: Integer;
begin
  if (AColonPos = -1) or not FSupportNamespaces then
  begin
    ALocalName := FNameTable.Add(FParsingState.Chars, FParsingState.CharPos, AEndNamePos - FParsingState.CharPos);
    Result := AddAttribute(ALocalName, '', ALocalName);
  end
  else
  begin
    FAttributeNeedNamespaceLookup := True;
    AStartPos := FParsingState.CharPos;
    APrefixLen := AColonPos - AStartPos;
    if (APrefixLen = Length(FLastPrefix)) and StrEqual(FParsingState.Chars, AStartPos, APrefixLen, FLastPrefix) then
      Result := AddAttribute(FNameTable.Add(FParsingState.Chars, AColonPos + 1, AEndNamePos - AColonPos - 1), FLastPrefix, '')
    else
    begin
      APrefix := FNameTable.Add(FParsingState.Chars, AStartPos, APrefixLen);
      FLastPrefix := APrefix;
      Result := AddAttribute(FNameTable.Add(FParsingState.Chars, AColonPos + 1, AEndNamePos - AColonPos - 1), APrefix, '');
    end;
  end;
end;

function TdxXmlTextReaderImpl.AddAttributeNoChecks(const AName: string; AAttrDepth: Integer): TdxNodeData;
var
  ANewAttr: TdxNodeData;
begin
  ANewAttr := AddNode(FIndex + FAttributeCount + 1, AAttrDepth);
  ANewAttr.SetNamedNode(TdxXmlNodeType.Attribute, FNameTable.Add(AName));
  Inc(FAttributeCount);
  Result := ANewAttr;
end;

function TdxXmlTextReaderImpl.LookupNamespace(ANode: TdxNodeData): string;
begin
  Result := FNamespaceManager.LookupNamespace(ANode.Prefix);
  if Result = '' then
    Throw(SXmlUnknownNs, ANode.Prefix, ANode.LineNo, ANode.LinePos);
end;

procedure TdxXmlTextReaderImpl.ElementNamespaceLookup;
begin
  Assert(FCurrentNode.&type = TdxXmlNodeType.Element);
  if FCurrentNode.Prefix = '' then
    FCurrentNode.Namespace := FXmlContext.DefaultNamespace
  else
    FCurrentNode.Namespace := LookupNamespace(FCurrentNode);
end;

procedure TdxXmlTextReaderImpl.ParseAttributeValueSlow(ACurPosition: Integer; AQuoteChar: Char; AAttribute: TdxNodeData);
label
  LblReadData;
var
  APosition: Integer;
  AChars: TCharArray;
  AValueChunkLineInfo, AEntityLineInfo: TdxLineInfo;
  ACh: Char;
begin
  APosition := ACurPosition;
  AChars := FParsingState.Chars;

  AValueChunkLineInfo := TdxLineInfo.Create(FParsingState.LineNo, FParsingState.LinePos);

  Assert(FStringBuilder.Length = 0);

  while True do
  begin
    while (TdxXmlCharType.CharProperties[AChars[APosition]] and TdxXmlCharType.AttrValue) <> 0 do
      Inc(APosition);

    if APosition - FParsingState.CharPos > 0 then
    begin
      FStringBuilder.Append(AChars, FParsingState.CharPos, APosition - FParsingState.CharPos);
      FParsingState.CharPos := APosition;
    end;

    if AChars[APosition] = AQuoteChar then
      Break
    else
    begin
      case AChars[APosition] of
        #$000A:
          begin
            Inc(APosition);
            OnNewLine(APosition);
            if FNormalize then
            begin
              FStringBuilder.Append(#$0020);
              Inc(FParsingState.CharPos);
            end;
            Continue;
          end;
        #$000D:
          begin
            if AChars[APosition + 1] = #$000A then
            begin
              Inc(APosition, 2);
              if FNormalize then
              begin
                if FParsingState.EolNormalized then
                  FStringBuilder.Append('  ')
                else
                  FStringBuilder.Append(#$0020);
                FParsingState.CharPos := APosition;
              end;
            end
            else
              if (APosition + 1 < FParsingState.CharsUsed) or FParsingState.IsEof then
              begin
                Inc(APosition);
                if FNormalize then
                begin
                  FStringBuilder.Append(#$0020);
                  FParsingState.CharPos := APosition;
                end;
              end
              else
                goto LblReadData;
            OnNewLine(APosition);
            Continue;
          end;
        #$0009:
          begin
            Inc(APosition);
            if FNormalize then
            begin
              FStringBuilder.Append(#$0020);
              Inc(FParsingState.CharPos);
            end;
            Continue;
          end;
        '"', #$00027, '>':
          begin
            Inc(APosition);
            Continue;
          end;
        '<':
          Throw(APosition, SXmlBadAttributeChar, EdxXmlException.BuildCharExceptionArgs('<', #0));
        '&':
          begin
            if APosition - FParsingState.CharPos > 0 then
              FStringBuilder.Append(AChars, FParsingState.CharPos, APosition - FParsingState.CharPos);
            FParsingState.CharPos := APosition;

            AEntityLineInfo := TdxLineInfo.Create(FParsingState.LineNo, FParsingState.LinePos + 1);

            if not (HandleEntityReference(True, TEntityExpandType.All, APosition) in
              [TEntityType.CharacterDec, TEntityType.CharacterHex, TEntityType.CharacterNamed]) then
              APosition := FParsingState.CharPos;
            AChars := FParsingState.Chars;
            Continue;
          end;
        else
        begin
          if APosition = FParsingState.CharsUsed then
            goto LblReadData
          else
          begin
            ACh := AChars[APosition];
            if TdxXmlCharType.IsHighSurrogate(ACh) then
            begin
              if APosition + 1 = FParsingState.CharsUsed then
                goto LblReadData;
              Inc(APosition);
              if TdxXmlCharType.IsLowSurrogate(AChars[APosition]) then
              begin
                Inc(APosition);
                Continue;
              end;
            end;
            ThrowInvalidChar(AChars, FParsingState.CharsUsed, APosition);
            Break;
          end;
        end;
      end;
    end;

  LblReadData:
    if ReadData = 0 then
    begin
      if FParsingState.CharsUsed - FParsingState.CharPos > 0 then
      begin
        if FParsingState.Chars[FParsingState.CharPos] <> #$000D then
        begin
          Assert(False, 'We should never get to this point.');
          Throw(SXmlUnexpectedEOF1);
        end;
        Assert(FParsingState.IsEof);
      end
      else
      begin
        Throw(SDTDNotImplemented);
      end;
    end;

    APosition := FParsingState.CharPos;
    AChars := FParsingState.Chars;
  end;

  FParsingState.CharPos := APosition + 1;

  AAttribute.SetValue(FStringBuilder.ToString);
  FStringBuilder.Length := 0;
end;

procedure TdxXmlTextReaderImpl.PushXmlContext;
begin
  FXmlContext := TXmlContext.Create(FXmlContext);
  FCurrentNode.XmlContextPushed := True;
end;

procedure TdxXmlTextReaderImpl.PopElementContext;
begin
  FNamespaceManager.PopScope;
  if FCurrentNode.XmlContextPushed then
    PopXmlContext;
end;

procedure TdxXmlTextReaderImpl.PopXmlContext;
var
  ACurrentXmlContext: TXmlContext;
begin
  Assert(FCurrentNode.XmlContextPushed);
  ACurrentXmlContext := FXmlContext;
  FXmlContext := FXmlContext.PreviousContext;
  FCurrentNode.XmlContextPushed := False;
  ACurrentXmlContext.Free;
end;

procedure TdxXmlTextReaderImpl.AddNamespace(const APrefix, AUri: string; AAttribute: TdxNodeData);
begin
  if AUri = TdxXmlReservedNs.NsXmlNs then
  begin
    if APrefix = XmlNs then
      Throw(SXmlXmlnsPrefix, AAttribute.LineInfo2.LineNo, AAttribute.LineInfo2.LinePos)
    else
      Throw(SXmlNamespaceDeclXmlXmlns, APrefix, AAttribute.LineInfo2.LineNo, AAttribute.LineInfo2.LinePos);
  end
  else
    if AUri = TdxXmlReservedNs.NsXml then
    begin
      if APrefix <> Xml then
        Throw(SXmlNamespaceDeclXmlXmlns, APrefix, AAttribute.LineInfo2.LineNo, AAttribute.LineInfo2.LinePos);
    end;
  if (AUri = '') and (APrefix <> '') then
    Throw(SXmlBadNamespaceDecl, AAttribute.LineInfo.LineNo, AAttribute.LineInfo.LinePos);

  try
    FNamespaceManager.AddNamespace(APrefix, AUri);
  except
    on E: Exception do
      ReThrow(E, AAttribute.LineInfo.LineNo, AAttribute.LineInfo.LinePos);
  end;
end;

procedure TdxXmlTextReaderImpl.OnDefaultNamespaceDecl(AAttribute: TdxNodeData);
var
  ANamespace: string;
begin
  if not FSupportNamespaces then
    Exit;

  ANamespace := FNameTable.Add(AAttribute.StringValue);
  AAttribute.Namespace := FNameTable.Add(TdxXmlReservedNs.NsXmlNs);

  if not FCurrentNode.XmlContextPushed then
    PushXmlContext;

  FXmlContext.DefaultNamespace := ANamespace;
  AddNamespace('', ANamespace, AAttribute);
end;

procedure TdxXmlTextReaderImpl.OnNamespaceDecl(AAttribute: TdxNodeData);
var
  ANamespace: string;
begin
  if not FSupportNamespaces then
    Exit;
  ANamespace := FNameTable.Add(AAttribute.StringValue);
  if ANamespace = '' then
    Throw(SXmlBadNamespaceDecl, AAttribute.LineInfo2.LineNo, AAttribute.LineInfo2.LinePos - 1);
  AddNamespace(AAttribute.LocalName, ANamespace, AAttribute);
end;

procedure TdxXmlTextReaderImpl.OnXmlReservedAttribute(AAttribute: TdxNodeData);
var
  AValue: string;
begin
  if AAttribute.LocalName = 'space' then
  begin
    if not FCurrentNode.XmlContextPushed then
      PushXmlContext;
    AValue := Trim(AAttribute.StringValue);
    if AValue = 'preserve' then
      FXmlContext.XmlSpace := TdxXmlSpace.Preserve
    else
      if AValue = 'default' then
        FXmlContext.XmlSpace := TdxXmlSpace.Default
      else
        Throw(SXmlInvalidXmlSpace, AAttribute.StringValue, AAttribute.LineInfo.LineNo, AAttribute.LineInfo.LinePos);
  end
  else
    if AAttribute.LocalName = 'lang' then
    begin
      if not FCurrentNode.XmlContextPushed then
        PushXmlContext;
      FXmlContext.XmlLang := AAttribute.StringValue;
    end;
end;

procedure TdxXmlTextReaderImpl.ParseAttributes;
label
  lbContinueParseName, LblReadData, lbEnd;
var
  APosition, ALineNoDelta, AStartNameCharSize, AAttrNameLinePos, AColonPos: Integer;
  AChars: TCharArray;
  AAttr: TdxNodeData;
  ATmpCh0, ATmpCh1, ATmpCh2, AQuoteChar, ATmpCh3: Char;
begin
  APosition := FParsingState.CharPos;
  AChars := FParsingState.Chars;

  Assert(FAttributeCount = 0);

  while True do
  begin
    ALineNoDelta := 0;

    ATmpCh0 := AChars[APosition];
    while (TdxXmlCharType.CharProperties[ATmpCh0] and TdxXmlCharType.Whitespace) <> 0 do
    begin
      if ATmpCh0 = #$000A then
      begin
        OnNewLine(APosition + 1);
        Inc(ALineNoDelta);
      end
      else
        if ATmpCh0 = #$000D then
        begin
          if AChars[APosition + 1] = #$000A then
          begin
            OnNewLine(APosition + 2);
            Inc(ALineNoDelta);
            Inc(APosition);
          end
          else
            if APosition + 1 <> FParsingState.CharsUsed then
            begin
              OnNewLine(APosition + 1);
              Inc(ALineNoDelta);
            end
            else
            begin
              FParsingState.CharPos := APosition;
              goto LblReadData;
            end;
        end;
      Inc(APosition);
      ATmpCh0 := AChars[APosition];
    end;

    AStartNameCharSize := 0;
    ATmpCh1 := AChars[APosition];
    if (TdxXmlCharType.CharProperties[ATmpCh1] and TdxXmlCharType.NCStartNameSC) <> 0 then
      AStartNameCharSize := 1;

    if AStartNameCharSize = 0 then
    begin
      if ATmpCh1 = '>' then
      begin
        Assert(FCurrentNode.&type = TdxXmlNodeType.Element);
        FParsingState.CharPos := APosition + 1;
        FParsingFunction := TParsingFunction.MoveToElementContent;
        goto lbEnd;
      end
      else
        if ATmpCh1 = '/' then
        begin
          Assert(FCurrentNode.&type = TdxXmlNodeType.Element);
          if APosition + 1 = FParsingState.CharsUsed then
            goto LblReadData;

          if AChars[APosition + 1] = '>' then
          begin
            FParsingState.CharPos := APosition + 2;
            FCurrentNode.IsEmptyElement := True;
            FNextParsingFunction := FParsingFunction;
            FParsingFunction := TParsingFunction.PopEmptyElementContext;
            goto lbEnd;
          end
          else
            ThrowUnexpectedToken(APosition + 1, '>');
        end
        else
          if APosition = FParsingState.CharsUsed then
            goto LblReadData
          else
            if (ATmpCh1 <> ':') or (FSupportNamespaces) then
              Throw(APosition, SXmlBadStartNameChar, EdxXmlException.BuildCharExceptionArgs(AChars, FParsingState.CharsUsed, APosition));
    end;

    if APosition = FParsingState.CharPos then
      ThrowExpectingWhitespace(APosition);
    FParsingState.CharPos := APosition;

    AAttrNameLinePos := FParsingState.LinePos;
    AColonPos := -1;

    Inc(APosition, AStartNameCharSize);

lbContinueParseName:
    repeat
      ATmpCh2 := AChars[APosition];
      if (TdxXmlCharType.CharProperties[ATmpCh2] and TdxXmlCharType.NCNameSC) <> 0 then
        Inc(APosition)
      else
        Break;
    until False;
    if ATmpCh2 = ':' then
    begin
      if AColonPos <> -1 then
      begin
        if FSupportNamespaces then
          Throw(APosition, SXmlBadNameChar, EdxXmlException.BuildCharExceptionArgs(':', #0))
        else
        begin
          Inc(APosition);
          goto lbContinueParseName;
        end;
      end
      else
      begin
        AColonPos := APosition;
        Inc(APosition);

        if (TdxXmlCharType.CharProperties[AChars[APosition]] and TdxXmlCharType.NCStartNameSC) <> 0 then
        begin
          Inc(APosition);
          goto lbContinueParseName;
        end;
        APosition := ParseQName(AColonPos);
        AChars := FParsingState.Chars;
      end;
    end
    else
      if APosition + 1 >= FParsingState.CharsUsed then
      begin
        APosition := ParseQName(AColonPos);
        AChars := FParsingState.Chars;
      end;

    AAttr := AddAttribute(APosition, AColonPos);
    AAttr.SetLineInfo(FParsingState.LineNo, AAttrNameLinePos);
    if AChars[APosition] <> '=' then
    begin
      FParsingState.CharPos := APosition;
      EatWhitespaces(nil);
      APosition := FParsingState.CharPos;
      if AChars[APosition] <> '=' then
        ThrowUnexpectedToken('=');
    end;
    Inc(APosition);
    AQuoteChar := AChars[APosition];
    if (AQuoteChar <> '"') and (AQuoteChar <> #$27) then
    begin
      FParsingState.CharPos := APosition;
      EatWhitespaces(nil);
      APosition := FParsingState.CharPos;
      AQuoteChar := AChars[APosition];
      if (AQuoteChar <> '"') and (AQuoteChar <> #$27) then
        ThrowUnexpectedToken('"', #$27);
    end;
    Inc(APosition);
    FParsingState.CharPos := APosition;

    AAttr.QuoteChar := AQuoteChar;
    AAttr.SetLineInfo2(FParsingState.LineNo, FParsingState.LinePos);

    repeat
      ATmpCh3 := AChars[APosition];
      if (TdxXmlCharType.CharProperties[ATmpCh3] and TdxXmlCharType.AttrValue) = 0 then
        Break;
      Inc(APosition);
    until False;

    if ATmpCh3 = AQuoteChar then
    begin
      AAttr.SetValue(AChars, FParsingState.CharPos, APosition - FParsingState.CharPos);
      Inc(APosition);
      FParsingState.CharPos := APosition;
    end
    else
    begin
      ParseAttributeValueSlow(APosition, AQuoteChar, AAttr);
      APosition := FParsingState.CharPos;
      AChars := FParsingState.Chars;
    end;
    if AAttr.Prefix = '' then
    begin
      if AAttr.LocalName = XmlNs then
        OnDefaultNamespaceDecl(AAttr);
    end
    else
    begin
      if AAttr.Prefix = XmlNs then
        OnNamespaceDecl(AAttr)
      else
        if AAttr.Prefix = Xml then
          OnXmlReservedAttribute(AAttr);
    end;
    Continue;

LblReadData:
    Dec(FParsingState.LineNo, ALineNoDelta);
    if ReadData <> 0 then
    begin
      APosition := FParsingState.CharPos;
      AChars := FParsingState.Chars;
    end
    else
      ThrowUnclosedElements;
  end;

lbEnd:
  ElementNamespaceLookup;
  if FAttributeNeedNamespaceLookup then
  begin
    AttributeNamespaceLookup;
    FAttributeNeedNamespaceLookup := False;
  end;
  if FAttributeDuplicateWalkCount >= MaxAttrDuplWalkCount then
    NotImplemented;
end;

procedure TdxXmlTextReaderImpl.ParseElement;
label
  ContinueStartName, ContinueName, ParseQNameSlow, SetElement;
var
  APosition, AColonPos, AStartPos, APrefixLen: Integer;
  AChars: TCharArray;
  ACh: Char;
  AIsWs: Boolean;
begin
  APosition := FParsingState.CharPos;
  AChars := FParsingState.Chars;
  AColonPos := -1;

  FCurrentNode.SetLineInfo(FParsingState.LineNo, FParsingState.LinePos);

ContinueStartName:
  if (TdxXmlCharType.CharProperties[AChars[APosition]] and TdxXmlCharType.NCStartNameSC) <> 0 then
    Inc(APosition)
  else
    goto ParseQNameSlow;

ContinueName:
  while (TdxXmlCharType.CharProperties[AChars[APosition]] and TdxXmlCharType.NCNameSC) <> 0 do
    Inc(APosition);
  if AChars[APosition] = ':' then
  begin
    if AColonPos <> -1 then
    begin
      if FSupportNamespaces then
        Throw(APosition, SXmlBadNameChar, EdxXmlException.BuildCharExceptionArgs(':', #0))
      else
      begin
        Inc(APosition);
        goto ContinueName;
      end;
    end
    else
    begin
      AColonPos := APosition;
      Inc(APosition);
      goto ContinueStartName;
    end;
  end
  else
    if APosition + 1 < FParsingState.CharsUsed then
      goto SetElement;

ParseQNameSlow:
  APosition := ParseQName(AColonPos);
  AChars := FParsingState.Chars;

SetElement:
  FNamespaceManager.PushScope;

  if (AColonPos = -1) or not FSupportNamespaces then
    FCurrentNode.SetNamedNode(TdxXmlNodeType.Element, FNameTable.Add(AChars, FParsingState.CharPos, APosition - FParsingState.CharPos))
  else
  begin
    AStartPos := FParsingState.CharPos;
    APrefixLen := AColonPos - AStartPos;
    if (APrefixLen = Length(FLastPrefix)) and StrEqual(AChars, AStartPos, APrefixLen, FLastPrefix) then
      FCurrentNode.SetNamedNode(TdxXmlNodeType.Element, FNameTable.Add(AChars, AColonPos + 1, APosition - AColonPos - 1), FLastPrefix, '')
    else
    begin
      FCurrentNode.SetNamedNode(TdxXmlNodeType.Element, FNameTable.Add(AChars, AColonPos + 1, APosition - AColonPos - 1),
        FNameTable.Add(AChars, FParsingState.CharPos, APrefixLen), '');
      FLastPrefix := FCurrentNode.Prefix;
    end;
  end;

  ACh := AChars[APosition];

  AIsWs := (TdxXmlCharType.CharProperties[ACh] and TdxXmlCharType.Whitespace) <> 0;

  if AIsWs then
  begin
    FParsingState.CharPos := APosition;
    ParseAttributes;
    Exit;
  end
  else
  begin
    if ACh = '>' then
    begin
      FParsingState.CharPos := APosition + 1;
      FParsingFunction := TParsingFunction.MoveToElementContent;
    end
    else
      if ACh = '/' then
      begin
        if APosition + 1 = FParsingState.CharsUsed then
        begin
          FParsingState.CharPos := APosition;
          if ReadData = 0 then
            Throw(APosition, SXmlUnexpectedEOF, '>');

          APosition := FParsingState.CharPos;
          AChars := FParsingState.Chars;
        end;
        if AChars[APosition + 1] = '>' then
        begin
          FCurrentNode.IsEmptyElement := True;
          FNextParsingFunction := FParsingFunction;
          FParsingFunction := TParsingFunction.PopEmptyElementContext;
          FParsingState.CharPos := APosition + 2;
        end
        else
          ThrowUnexpectedToken(APosition, '>');
      end
      else
        Throw(APosition, SXmlBadNameChar, EdxXmlException.BuildCharExceptionArgs(AChars, FParsingState.CharsUsed, APosition));

    ElementNamespaceLookup;
  end;
end;

function TdxXmlTextReaderImpl.ParseElementContent: Boolean;
label
  LReadData;
var
  APosition: Integer;
  AChars: TCharArray;
begin
  while True do
  begin
    APosition := FParsingState.CharPos;
    AChars := FParsingState.Chars;

    case AChars[APosition] of
      '<':
        case AChars[APosition + 1] of
          '?':
            begin
              FParsingState.CharPos := APosition + 2;
              if ParsePI then
                Exit(True);
              Continue;
            end;
          '!':
            begin
              Inc(APosition, 2);
              if FParsingState.CharsUsed - APosition < 2 then
                goto LReadData;

              if AChars[APosition] = '-' then
              begin
                if AChars[APosition + 1] = '-' then
                begin
                  FParsingState.CharPos := APosition + 2;
                  if ParseComment then
                    Exit(True);
                  Continue;
                end
                else
                  ThrowUnexpectedToken(APosition + 1, '-');
              end
              else
                if AChars[APosition] = '[' then
                begin
                  Inc(APosition);
                  if FParsingState.CharsUsed - APosition < 6 then
                    goto LReadData;
                  if StrEqual(AChars, APosition, 6, 'CDATA[') then
                  begin
                    FParsingState.CharPos := APosition + 6;
                    ParseCData;
                    Exit(True);
                  end
                  else
                    ThrowUnexpectedToken(APosition, 'CDATA[');
                end
                else
                begin
                  if ParseUnexpectedToken(APosition) = 'DOCTYPE' then
                    Throw(SXMLBadDTDLocation)
                  else
                    ThrowUnexpectedToken(APosition, '<!--', '<[CDATA[');
                end;
            end;
          '/':
            begin
              FParsingState.CharPos := APosition + 2;
              ParseEndElement;
              Exit(True);
            end;
          else
            if APosition + 1 = FParsingState.CharsUsed then
              goto LReadData
            else
            begin
              FParsingState.CharPos := APosition + 1;
              ParseElement;
              Exit(True);
            end;
        end;
      '&':
        begin
          if ParseText then
            Exit(True);
          Continue;
        end
      else
        if APosition = FParsingState.CharsUsed then
          goto LReadData
        else
        begin
          if ParseText then
            Exit(True);
          Continue;
        end;
    end;


  LReadData:
    if ReadData = 0 then
    begin
      if FParsingState.CharsUsed - FParsingState.CharPos <> 0 then
        ThrowUnclosedElements;
      if (FIndex = 0) and (FFragmentType <> TdxXmlNodeType.Document) then
      begin
        OnEof;
        Exit(False);
      end;
      ThrowUnclosedElements;
    end;
  end;
end;

procedure TdxXmlTextReaderImpl.ParseEndElement;
label
  LblReadData;
var
  AStartTagNode: TdxNodeData;
  APrefLen, ALocLen, ANameLen, AColonPos, APosition: Integer;
  AChars: TCharArray;
  AEndTagLineInfo: TdxLineInfo;
  ATmpCh: Char;
begin
  AStartTagNode := FNodes[FIndex - 1];

  APrefLen := Length(AStartTagNode.Prefix);
  ALocLen := Length(AStartTagNode.LocalName);

  while FParsingState.CharsUsed - FParsingState.CharPos < APrefLen + ALocLen + 1 do
    if ReadData = 0 then
      Break;

  AChars := FParsingState.Chars;
  if Length(AStartTagNode.Prefix) = 0 then
  begin
    if not StrEqual(AChars, FParsingState.CharPos, ALocLen, AStartTagNode.LocalName) then
      ThrowTagMismatch(AStartTagNode);
    ANameLen := ALocLen;
  end
  else
  begin
    AColonPos := FParsingState.CharPos + APrefLen;
    if (not StrEqual(AChars, FParsingState.CharPos, APrefLen, AStartTagNode.Prefix)) or (AChars[AColonPos] <> ':') or
       (not StrEqual(AChars, AColonPos + 1, ALocLen, AStartTagNode.LocalName)) then
      ThrowTagMismatch(AStartTagNode);
    ANameLen := ALocLen + APrefLen + 1;
  end;

  AEndTagLineInfo := TdxLineInfo.Create(FParsingState.LineNo, FParsingState.LinePos);

  repeat
    APosition := FParsingState.CharPos + ANameLen;
    AChars := FParsingState.Chars;

    if APosition = FParsingState.CharsUsed then
      goto LblReadData;
    if (((TdxXmlCharType.CharProperties[AChars[APosition]] and TdxXmlCharType.NCNameSC) <> 0) or
       (AChars[APosition] = ':')) then
      ThrowTagMismatch(AStartTagNode);

    if AChars[APosition] <> '>' then
    begin
      ATmpCh := AChars[APosition];
      while TdxXmlCharType.IsWhiteSpace(ATmpCh) do
      begin
        Inc(APosition);
        case ATmpCh of
          #$000A:
            begin
              OnNewLine(APosition);
              Continue;
            end;
          #$000D:
            begin
              if AChars[APosition] = #$000A then
                Inc(APosition)
              else
                if (APosition = FParsingState.CharsUsed) and not FParsingState.IsEof then
                  Break;
              OnNewLine(APosition);
              Continue;
            end;
        end;
      end;
    end;

    if AChars[APosition] = '>' then
      Break
    else
      if APosition = FParsingState.CharsUsed then
        goto LblReadData
      else
        ThrowUnexpectedToken(APosition, '>');

    Assert(False, 'We should never get to this point.');

LblReadData:
    if ReadData = 0 then
      Throw('UnclosedElements');
  until False;

  Assert(FIndex > 0);
  Dec(FIndex);
  FCurrentNode := FNodes[FIndex];

  Assert(FCurrentNode = AStartTagNode);
  AStartTagNode.LineInfo := AEndTagLineInfo;
  AStartTagNode.&Type := TdxXmlNodeType.EndElement;
  FParsingState.CharPos := APosition + 1;

  if (FIndex > 0) then
    FNextParsingFunction := FParsingFunction
  else
    FNextParsingFunction := TParsingFunction.DocumentContent;
  FParsingFunction := TParsingFunction.PopElementContext;
end;

function TdxXmlTextReaderImpl.ParseName: Integer;
var
  AColonPos: Integer;
begin
  Result := ParseQName(False, 0, AColonPos);
end;

procedure TdxXmlTextReaderImpl.OnNewLine(APosition: Integer);
begin
  Inc(FParsingState.LineNo);
  FParsingState.LineStartPos := APosition - 1;
end;

function TdxXmlTextReaderImpl.EatWhitespaces(ASb: TStringBuilder): Integer;
var
  APosition, AWsCount, ATmp1, ATmp2, ATmp3: Integer;
  AChars: TCharArray;
begin
  APosition := FParsingState.CharPos;
  AWsCount := 0;
  AChars := FParsingState.Chars;

  while True do
  begin
    while True do
    begin
      case AChars[APosition] of
        #$000A:
          begin
            Inc(APosition);
            OnNewLine(APosition);
          end;
        #$000D:
          begin
            if AChars[APosition + 1] = #$000A then
            begin
              ATmp1 := APosition - FParsingState.CharPos;
              if (ASb <> nil) and not FParsingState.EolNormalized then
              begin
                if ATmp1 > 0 then
                begin
                  ASb.Append(AChars, FParsingState.CharPos, ATmp1);
                  Inc(AWsCount, ATmp1);
                end;
                FParsingState.CharPos := APosition + 1;
              end;
              Inc(APosition, 2);
            end
            else
              if (APosition + 1 < FParsingState.CharsUsed) or (FParsingState.IsEof) then
              begin
                if not FParsingState.EolNormalized then
                  AChars[APosition] := #$000A;
                Inc(APosition);
              end
              else
                Break;
            OnNewLine(APosition);
          end;
        #$0009, ' ':
          Inc(APosition);
        else
        begin
          if APosition = FParsingState.CharsUsed then
            Break
          else
          begin
            ATmp2 := APosition - FParsingState.CharPos;
            if ATmp2 > 0 then
            begin
              if ASb <> nil then
                ASb.Append(FParsingState.Chars, FParsingState.CharPos, ATmp2);
              FParsingState.CharPos := APosition;
              Inc(AWsCount, ATmp2);
            end;
            Exit(AWsCount);
          end;
        end;
      end;
    end;


    ATmp3 := APosition - FParsingState.CharPos;
    if ATmp3 > 0 then
    begin
      if ASb <> nil then
        ASb.Append(FParsingState.Chars, FParsingState.CharPos, ATmp3);
      FParsingState.CharPos := APosition;
      Inc(AWsCount, ATmp3);
    end;

    if ReadData = 0 then
    begin
      if FParsingState.CharsUsed - FParsingState.CharPos = 0 then
        Exit(AWsCount);

      if FParsingState.Chars[FParsingState.CharPos] <> #$000D then
      begin
        Assert(False, 'We should never get to this point.');
        Throw(SXmlUnexpectedEOF1);
      end;
      Assert(FParsingState.IsEof);
    end;
    APosition := FParsingState.CharPos;
    AChars := FParsingState.Chars;
  end;
end;

procedure TdxXmlTextReaderImpl.ShiftBuffer(ASourcePosition, ADestPosition, ACount: Integer);
begin
  Move(FParsingState.Chars[ASourcePosition], FParsingState.Chars[ADestPosition], ACount * SizeOf(Char));
end;

function TdxXmlTextReaderImpl.ParsePIValue(out AOutStartPosition, AOutEndPosition: Integer): Boolean;
var
  APosition, ARcount, ARpos: Integer;
  AChars: TCharArray;
  ATmpCh, ACh: Char;
begin
  if FParsingState.CharsUsed - FParsingState.CharPos < 2 then
    if ReadData = 0 then
      Throw(FParsingState.CharsUsed, SXmlUnexpectedEOF, 'PI');

  APosition := FParsingState.CharPos;
  AChars := FParsingState.Chars;
  ARcount := 0;
  ARpos := -1;

  while True do
  begin
    while True do
    begin
      ATmpCh := AChars[APosition];
      if not ((ATmpCh <> '?') and (TdxXmlCharType.CharProperties[ATmpCh] and TdxXmlCharType.Text <> 0)) then
        Break;
      Inc(APosition);
    end;

    case AChars[APosition] of
      '?':
        begin
          if AChars[APosition + 1] = '>' then
          begin
            if ARcount > 0 then
            begin
              Assert(not FParsingState.EolNormalized);
              ShiftBuffer(ARpos + ARcount, ARpos, APosition - ARpos - ARcount);
              AOutEndPosition := APosition - ARcount;
            end
            else
              AOutEndPosition := APosition;
            AOutStartPosition := FParsingState.CharPos;
            FParsingState.CharPos := APosition + 2;
            Exit(True);
          end
          else
            if APosition + 1 = FParsingState.CharsUsed then
              Break
            else
              Inc(APosition);
        end;
      #$000A:
        begin
          Inc(APosition);
          OnNewLine(APosition);
        end;
      #$000D:
        begin
          if AChars[APosition + 1] = #$000A then
          begin
            if not FParsingState.EolNormalized and (FParsingMode = TParsingMode.Full) then
            begin

              if APosition - FParsingState.CharPos > 0 then
              begin
                if ARcount = 0 then
                begin
                  ARcount := 1;
                  ARpos := APosition;
                end
                else
                begin
                  ShiftBuffer(ARpos + ARcount, ARpos, APosition - ARpos - ARcount);
                  ARpos := APosition - ARcount;
                  Inc(ARcount);
                end;
              end
              else
                Inc(FParsingState.CharPos);
            end;
            Inc(APosition, 2);
          end
          else
            if (APosition + 1 < FParsingState.CharsUsed) or FParsingState.IsEof then
            begin
              if not FParsingState.EolNormalized then
                AChars[APosition] := #$000A;
              Inc(APosition);
            end
            else
              Break;
          OnNewLine(APosition);
        end;
      '<', '&', ']', #$0009:
        Inc(APosition);
      else
      begin
        if APosition = FParsingState.CharsUsed then
          Break
        else
        begin
          ACh := AChars[APosition];
          if TdxXmlCharType.IsHighSurrogate(ACh) then
          begin
            if APosition + 1 = FParsingState.CharsUsed then
              Break;
            Inc(APosition);
            if TdxXmlCharType.IsLowSurrogate(AChars[APosition]) then
            begin
              Inc(APosition);
              Continue;
            end;
          end;
          ThrowInvalidChar(AChars, FParsingState.CharsUsed, APosition);
          Break;
        end;
      end;
    end;
  end;

  if ARcount > 0 then
  begin
    ShiftBuffer(ARpos + ARcount, ARpos, APosition - ARpos - ARcount);
    AOutEndPosition := APosition - ARcount;
  end
  else
    AOutEndPosition := APosition;

  AOutStartPosition := FParsingState.CharPos;
  FParsingState.CharPos := APosition;
  Result := False;
end;

function TdxXmlTextReaderImpl.ParsePI(APiInDtdStringBuilder: TStringBuilder): Boolean;
var
  ANameEndPos, AStartPos, AEndPos: Integer;
  ATarget: string;
  ACh: Char;
  ASb: TStringBuilder;
begin
  if FParsingMode = TParsingMode.Full then
    FCurrentNode.SetLineInfo(FParsingState.LineNo, FParsingState.LinePos);

  Assert(FStringBuilder.Length = 0);
  ANameEndPos := ParseName;
  ATarget := FNameTable.Add(FParsingState.Chars, FParsingState.CharPos, ANameEndPos - FParsingState.CharPos);

  if SameText(ATarget, 'xml') then
    Throw(IfThen(ATarget = 'xml', SXMLXmlDeclNotFirst, SXmlInvalidPIName), ATarget);

  FParsingState.CharPos := ANameEndPos;

  if APiInDtdStringBuilder = nil then
  begin
    if not FIgnorePIs and (FParsingMode = TParsingMode.Full) then
      FCurrentNode.SetNamedNode(TdxXmlNodeType.ProcessingInstruction, ATarget);
  end
  else
    APiInDtdStringBuilder.Append(ATarget);

  ACh := FParsingState.Chars[FParsingState.CharPos];
  Assert(FParsingState.CharPos < FParsingState.CharsUsed);

  if EatWhitespaces(APiInDtdStringBuilder) = 0 then
  begin
    if FParsingState.CharsUsed - FParsingState.CharPos < 2 then
      ReadData;
    if (ACh <> '?') or (FParsingState.Chars[FParsingState.CharPos + 1] <> '>') then
      Throw(SXmlBadNameChar, ConvertToConstArray(EdxXmlException.BuildCharExceptionArgs(FParsingState.Chars, FParsingState.CharsUsed,
        FParsingState.CharPos)));
  end;

  if ParsePIValue(AStartPos, AEndPos) then
  begin
    if APiInDtdStringBuilder = nil then
    begin
      if FIgnorePIs then
        Exit(False);
      if FParsingMode = TParsingMode.Full then
        FCurrentNode.SetValue(FParsingState.Chars, AStartPos, AEndPos - AStartPos);
    end
    else
      APiInDtdStringBuilder.Append(FParsingState.Chars, AStartPos, AEndPos - AStartPos);
  end
  else
  begin
    if APiInDtdStringBuilder = nil then
    begin
      if (FIgnorePIs) or (FParsingMode <> TParsingMode.Full) then
      begin
        while not ParsePIValue(AStartPos, AEndPos) do;
        Exit(False);
      end;
      ASb := FStringBuilder;
      Assert(FStringBuilder.Length = 0);
    end
    else
      ASb := APiInDtdStringBuilder;

    repeat
      ASb.Append(FParsingState.Chars, AStartPos, AEndPos - AStartPos);
    until ParsePIValue(AStartPos, AEndPos);
    ASb.Append(FParsingState.Chars, AStartPos, AEndPos - AStartPos);

    if APiInDtdStringBuilder = nil then
    begin
      FCurrentNode.SetValue(FStringBuilder.ToString);
      FStringBuilder.Length := 0;
    end;
  end;
  Result := True;
end;

function TdxXmlTextReaderImpl.ParseQName(out AColonPosition: Integer): Integer;
begin
  Result := ParseQName(True, 0, AColonPosition);
end;

function TdxXmlTextReaderImpl.ReadDataInName(var APosition: Integer): Boolean;
var
  AOffset: Integer;
begin
  AOffset := APosition - FParsingState.CharPos;
  Result := ReadData <> 0;
  APosition := FParsingState.CharPos + AOffset;
end;

function TdxXmlTextReaderImpl.ParseQName(AIsQName: Boolean; AStartOffset: Integer; out AColonPosition: Integer): Integer;
label
  ContinueStartName, ContinueName;
var
  AColonOffset, APosition: Integer;
  AChars: TCharArray;
begin
  AColonOffset := -1;
  APosition := FParsingState.CharPos + AStartOffset;

ContinueStartName:
  AChars := FParsingState.Chars;
  if TdxXmlCharType.CharProperties[AChars[APosition]] and TdxXmlCharType.NCStartNameSC <> 0 then
    Inc(APosition)
  else
  begin
    if APosition + 1 >= FParsingState.CharsUsed then
    begin
      if ReadDataInName(APosition) then
        goto ContinueStartName;

      Throw(APosition, SXmlUnexpectedEOF, 'Name');
    end;
    if (AChars[APosition] <> ':') or FSupportNamespaces then
      Throw(APosition, SXmlBadStartNameChar, EdxXmlException.BuildCharExceptionArgs(AChars,
        FParsingState.CharsUsed, APosition));
  end;

ContinueName:
  while TdxXmlCharType.CharProperties[AChars[APosition]] and TdxXmlCharType.NCNameSC <> 0 do
    Inc(APosition);

  if AChars[APosition] = ':' then
  begin
    if FSupportNamespaces then
    begin
      if (AColonOffset <> -1) or not AIsQName then
        Throw(APosition, SXmlBadNameChar, EdxXmlException.BuildCharExceptionArgs(':', #0));

      AColonOffset := APosition - FParsingState.CharPos;
      Inc(APosition);
      goto ContinueStartName;
    end
    else
    begin
      AColonOffset := APosition - FParsingState.CharPos;
      Inc(APosition);
      goto ContinueName;
    end;
  end
  else
    if APosition = FParsingState.CharsUsed then
    begin
      if ReadDataInName(APosition) then
      begin
        AChars := FParsingState.Chars;
        goto ContinueName;
      end;
      Throw(APosition, SXmlUnexpectedEOF, 'Name');
    end;

  if AColonOffset = -1 then
    AColonPosition := -1
  else
    AColonPosition := FParsingState.CharPos + AColonOffset;
  Result := APosition;
end;

function TdxXmlTextReaderImpl.ParseRootLevelWhitespace: Boolean;
var
  ANodeType: TdxXmlNodeType;
begin
  Assert(FStringBuilder.Length = 0);

  ANodeType := GetWhitespaceType;

  if ANodeType = TdxXmlNodeType.None then
  begin
    EatWhitespaces(nil);
    if (FParsingState.Chars[FParsingState.CharPos] = '<') or (FParsingState.CharsUsed - FParsingState.CharPos = 0) then
      Exit(False);
  end
  else
  begin
    FCurrentNode.SetLineInfo(FParsingState.LineNo, FParsingState.LinePos);
    EatWhitespaces(FStringBuilder);
    if (FParsingState.Chars[FParsingState.CharPos] = '<') or (FParsingState.CharsUsed - FParsingState.CharPos = 0) then
    begin
      if FStringBuilder.Length > 0 then
      begin
        FCurrentNode.SetValueNode(ANodeType, FStringBuilder.ToString);
        FStringBuilder.Length := 0;
        Exit(True);
      end;
      Exit(False);
    end;
  end;

  if TdxXmlCharType.IsCharData(FParsingState.Chars[FParsingState.CharPos]) then
    Throw(SXmlInvalidRootData)
  else
    ThrowInvalidChar(FParsingState.Chars, FParsingState.CharsUsed, FParsingState.CharPos);
  Result := False;
end;

function TdxXmlTextReaderImpl.GetWhitespaceType: TdxXmlNodeType;
begin
  if FWhitespaceHandling <> TdxWhitespaceHandling.None then
  begin
    if FXmlContext.XmlSpace = TdxXmlSpace.Preserve then
      Exit(TdxXmlNodeType.SignificantWhitespace);

    if FWhitespaceHandling = TdxWhitespaceHandling.All then
      Exit(TdxXmlNodeType.Whitespace);
  end;
  Result := TdxXmlNodeType.None;
end;

function TdxXmlTextReaderImpl.GetXmlSpace: TdxXmlSpace;
begin
  Result := FXmlContext.XmlSpace;
end;

function TdxXmlTextReaderImpl.GetTextNodeType(AOrChars: Integer): TdxXmlNodeType;
begin
  if AOrChars > $20 then
    Result := TdxXmlNodeType.Text
  else
    Result := GetWhitespaceType;
end;

function TdxXmlTextReaderImpl.GetValue: string;
begin
  if FParsingFunction >= TParsingFunction.PartialTextValue then
    if FParsingFunction = TParsingFunction.PartialTextValue then
    begin
      FinishPartialValue;
      FParsingFunction := FNextParsingFunction;
    end;
  Result := FCurrentNode.StringValue;
end;

function TdxXmlTextReaderImpl.ParseText: Boolean;
var
  AStartPos, AEndPos, AOrChars: Integer;
  ANodeType: TdxXmlNodeType;
  AFullValue: Boolean;
begin
  AOrChars := 0;

  if FParsingMode <> TParsingMode.Full then
  begin
    while not ParseText(AStartPos, AEndPos, AOrChars) do ;
    Exit(False);
  end;

  FCurrentNode.SetLineInfo(FParsingState.LineNo, FParsingState.LinePos);
  Assert(FStringBuilder.Length = 0);

  if ParseText(AStartPos, AEndPos, AOrChars) then
  begin
    if AEndPos - AStartPos = 0 then
      Exit(False);

    ANodeType := GetTextNodeType(AOrChars);
    if ANodeType = TdxXmlNodeType.None then
      Exit(False);

    Assert(AEndPos - AStartPos > 0);
    FCurrentNode.SetValueNode(ANodeType, FParsingState.Chars, AStartPos, AEndPos - AStartPos);
  end
  else
  begin
    if AOrChars > $20 then
    begin
      Assert(AEndPos - AStartPos > 0);
      FCurrentNode.SetValueNode(TdxXmlNodeType.Text, FParsingState.Chars, AStartPos, AEndPos - AStartPos);
      FNextParsingFunction := FParsingFunction;
      FParsingFunction := TParsingFunction.PartialTextValue;
      Exit(True);
    end;
    if AEndPos - AStartPos > 0 then
      FStringBuilder.Append(FParsingState.Chars, AStartPos, AEndPos - AStartPos);

    repeat
      AFullValue := ParseText(AStartPos, AEndPos, AOrChars);
      if AEndPos - AStartPos > 0 then
        FStringBuilder.Append(FParsingState.Chars, AStartPos, AEndPos - AStartPos);
    until not (not AFullValue and (AOrChars <= $20) and (FStringBuilder.Length < MinWhitespaceLookahedCount));
    if (FStringBuilder.Length < MinWhitespaceLookahedCount) then
      ANodeType := GetTextNodeType(AOrChars)
    else
      ANodeType := TdxXmlNodeType.Text;

    if ANodeType = TdxXmlNodeType.None then
    begin
      FStringBuilder.Length := 0;
      if not AFullValue then
        while not ParseText(AStartPos, AEndPos, AOrChars) do ;
      Exit(False);
    end;
    FCurrentNode.SetValueNode(ANodeType, FStringBuilder.ToString);
    FStringBuilder.Length := 0;
    if not AFullValue then
    begin
      FNextParsingFunction := FParsingFunction;
      FParsingFunction := TParsingFunction.PartialTextValue;
    end;
  end;

  Result := True;
end;

function TdxXmlTextReaderImpl.ParseNumericCharRefInline(AStartPosition: Integer; AExpand: Boolean;
  AInternalSubsetBuilder: TStringBuilder; out ACharCount: Integer; out AEntityType: TEntityType): Integer;
label
  Return;
var
  AVal, APosition, ADigitPos: Integer;
  AChars: TCharArray;
  ABadDigitExceptionString: string;
  ACh, ALow, AHigh: Char;
begin
  Assert((FParsingState.Chars[AStartPosition] = '&') and (FParsingState.Chars[AStartPosition + 1] = '#'));

  AVal := 0;
  ABadDigitExceptionString := '';
  AChars := FParsingState.Chars;
  APosition := AStartPosition + 2;
  ACharCount := 0;
  ADigitPos := 0;

  try
    if AChars[APosition] = 'x' then
    begin
      Inc(APosition);
      ADigitPos := APosition;
      ABadDigitExceptionString := SXmlBadHexEntity;
      while True do
      begin
        ACh := AChars[APosition];
        {$Q+}
        if (ACh >= '0') and (ACh <= '9') then
          AVal := AVal * 16 + Ord(ACh) - Ord('0')
        else
          if (ACh >= 'a') and (ACh <= 'f') then
            AVal := AVal * 16 + 10 + Ord(ACh) - Ord('a')
          else
            if (ACh >= 'A') and (ACh <= 'F') then
              AVal := AVal * 16 + 10 + Ord(ACh) - Ord('A')
            else
              Break;
        {$Q-}
        Inc(APosition);
      end;
      AEntityType := TEntityType.CharacterHex;
    end
    else
      if APosition < FParsingState.CharsUsed then
      begin
        ADigitPos := APosition;
        ABadDigitExceptionString := SXmlBadDecimalEntity;
        while (AChars[APosition] >= '0') and (AChars[APosition] <= '9') do
        begin
          {$Q+}
          AVal := AVal * 10 + Ord(AChars[APosition]) - Ord('0');
          {$Q-}
          Inc(APosition);
        end;
        AEntityType := TEntityType.CharacterDec;
      end
      else
      begin
        AEntityType := TEntityType.Skipped;
        Exit(-2);
      end;
  except
    on EOverflow do
      begin
        FParsingState.CharPos := APosition;
        AEntityType := TEntityType.Skipped;
        Throw(SXmlCharEntityOverflow, '');
      end;
  end;

  if (AChars[APosition] <> ';') or (ADigitPos = APosition) then
    if APosition = FParsingState.CharsUsed then
      Exit(-2)
    else
      Throw(APosition, ABadDigitExceptionString);

  if AVal <= Ord(High(Char)) then
  begin
    ACh := Char(AVal);
    if not TdxXmlCharType.IsCharData(ACh) and FCheckCharacters then
      Throw(IfThen(FParsingState.Chars[AStartPosition + 2] = 'x', AStartPosition + 3, AStartPosition + 2), SXmlInvalidCharacter,
        EdxXmlException.BuildCharExceptionArgs(ACh, #0));

    if AExpand then
    begin
      if AInternalSubsetBuilder <> nil then
        AInternalSubsetBuilder.Append(FParsingState.Chars, FParsingState.CharPos, APosition - FParsingState.CharPos + 1);
      AChars[APosition] := ACh;
    end;
    ACharCount := 1;
  end
  else
  begin
    TdxXmlCharType.SplitSurrogateChar(AVal, ALow, AHigh);

    if FNormalize then
    begin
      if TdxXmlCharType.IsHighSurrogate(AHigh) then
        if TdxXmlCharType.IsLowSurrogate(ALow) then
          goto Return;
      Throw(IfThen(FParsingState.Chars[AStartPosition + 2] = 'x', AStartPosition + 3, AStartPosition + 2), SXmlInvalidCharacter,
        EdxXmlException.BuildCharExceptionArgs(AHigh, ALow));
    end;

Return:
    Assert(APosition > 0);
    if AExpand then
    begin
      if AInternalSubsetBuilder <> nil then
        AInternalSubsetBuilder.Append(FParsingState.Chars, FParsingState.CharPos, APosition - FParsingState.CharPos + 1);
      AChars[APosition - 1] := AHigh;
      AChars[APosition] := ALow;
    end;
    ACharCount := 2;
  end;
  Result := APosition + 1;
end;

function TdxXmlTextReaderImpl.ParseNamedCharRefInline(AStartPosition: Integer; AExpand: Boolean;
  AInternalSubsetBuilder: TStringBuilder): Integer;
label
  FoundCharRef;
var
  APosition: Integer;
  AChars: TCharArray;
  ACh: Char;
begin
  Assert(AStartPosition < FParsingState.CharsUsed);
  Assert(FParsingState.Chars[AStartPosition] = '&');
  Assert(FParsingState.Chars[AStartPosition + 1] <> '#');

  APosition := AStartPosition + 1;
  AChars := FParsingState.Chars;

  case AChars[APosition] of
    'a':
      begin
        Inc(APosition);

        if AChars[APosition] = 'm' then
        begin
          if FParsingState.CharsUsed - APosition >= 3 then
          begin
            if (AChars[APosition + 1] = 'p') and (AChars[APosition + 2] = ';') then
            begin
              Inc(APosition, 3);
              ACh := '&';
              goto FoundCharRef;
            end
            else
              Exit(-1);
          end;
        end
        else
          if AChars[APosition] = 'p' then
          begin
            if FParsingState.CharsUsed - APosition >= 4 then
            begin
              if (AChars[APosition + 1] = 'o') and (AChars[APosition + 2] = 's') and (AChars[APosition + 3] = ';') then
              begin
                Inc(APosition, 4);
                ACh := #$27;
                goto FoundCharRef;
              end
              else
                Exit(-1);
            end;
          end
          else
            if APosition < FParsingState.CharsUsed then
              Exit(-1);
      end;
    'q':
      if FParsingState.CharsUsed - APosition >= 5 then
      begin
        if (AChars[APosition + 1] = 'u') and (AChars[APosition + 2] = 'o') and (AChars[APosition + 3] = 't') and (AChars[APosition + 4] = ';') then
        begin
          Inc(APosition, 5);
          ACh := '"';
          goto FoundCharRef;
        end
        else
          Exit(-1);
      end;
    'l':
      if FParsingState.CharsUsed - APosition >= 3 then
      begin
        if (AChars[APosition + 1] = 't') and (AChars[APosition + 2] = ';') then
        begin
          Inc(APosition, 3);
          ACh := '<';
          goto FoundCharRef;
        end
        else
          Exit(-1);
      end;
    'g':
      if FParsingState.CharsUsed - APosition >= 3 then
      begin
        if (AChars[APosition + 1] = 't') and (AChars[APosition + 2] = ';') then
        begin
          Inc(APosition, 3);
          ACh := '>';
          goto FoundCharRef;
        end
        else
          Exit(-1);
      end;
    else
      Exit(-1);
  end;
  Exit(-2);

FoundCharRef:
  Assert(APosition > 0);
  if AExpand then
  begin
    if AInternalSubsetBuilder <> nil then
      AInternalSubsetBuilder.Append(FParsingState.Chars, FParsingState.CharPos, APosition - FParsingState.CharPos);
    FParsingState.Chars[APosition - 1] := ACh;
  end;
  Result := APosition;
end;

function TdxXmlTextReaderImpl.ParseCharRefInline(AStartPosition: Integer; out ACharCount: Integer;
  out AEntityType: TEntityType): Integer;
begin
  Assert(FParsingState.Chars[AStartPosition] = '&');
  if FParsingState.Chars[AStartPosition + 1] = '#' then
    Result := ParseNumericCharRefInline(AStartPosition, True, nil, ACharCount, AEntityType)
  else
  begin
    ACharCount := 1;
    AEntityType := TEntityType.CharacterNamed;
    Result := ParseNamedCharRefInline(AStartPosition, True, nil);
  end;
end;

function TdxXmlTextReaderImpl.ParseNumericCharRef(AExpand: Boolean; AInternalSubsetBuilder: TStringBuilder;
  out AEntityType: TEntityType): Integer;
var
  ANewPos, ACharCount: Integer;
begin
  while True do
  begin
    ANewPos := ParseNumericCharRefInline(FParsingState.CharPos, AExpand, AInternalSubsetBuilder, ACharCount, AEntityType);
    case ANewPos of
      -2:
        begin
          if ReadData = 0 then
            Throw(SXmlUnexpectedEOF);
          Assert(FParsingState.Chars[FParsingState.CharPos] = '&');
        end;
      else
      begin
        if AExpand then
          FParsingState.CharPos := ANewPos - ACharCount;
        Exit(ANewPos);
      end;
    end;
  end;
end;

function TdxXmlTextReaderImpl.ParseNamedCharRef(AExpand: Boolean; AInternalSubsetBuilder: TStringBuilder): Integer;
var
  ANewPos: Integer;
begin
  while True do
  begin
    ANewPos := ParseNamedCharRefInline(FParsingState.CharPos, AExpand, AInternalSubsetBuilder);
    case ANewPos of
      -1:
        Exit(-1);
      -2:
        begin
          if ReadData = 0 then
            Exit(-1);
          Assert(FParsingState.Chars[FParsingState.CharPos] = '&');
          Continue;
        end;
      else
      begin
        if AExpand then
          FParsingState.CharPos := ANewPos - 1;
        Exit(ANewPos);
      end;
    end;
  end;
end;

function TdxXmlTextReaderImpl.HandleEntityReference(AIsInAttributeValue: Boolean; AExpandType: TEntityExpandType;
  out ACharRefEndPos: Integer): TEntityType;
var
  AEntityType: TEntityType;
begin
  Assert(FParsingState.Chars[FParsingState.CharPos] = '&');

  if FParsingState.CharPos + 1 = FParsingState.CharsUsed then
  begin
    if ReadData = 0 then
      Throw(SXmlUnexpectedEOF1);
  end;
  if FParsingState.Chars[FParsingState.CharPos + 1] = '#' then
  begin
    ACharRefEndPos := ParseNumericCharRef(AExpandType <> TEntityExpandType.OnlyGeneral, nil, AEntityType);
    Assert((AEntityType = TEntityType.CharacterDec) or (AEntityType = TEntityType.CharacterHex));
    Result := AEntityType;
  end
  else
  begin
    ACharRefEndPos := ParseNamedCharRef(AExpandType <> TEntityExpandType.OnlyGeneral, nil);
    if ACharRefEndPos >= 0 then
      Exit(TEntityType.CharacterNamed);

    Throw(SDTDNotImplemented);
    Result := TEntityType.Skipped;
  end;
end;

function TdxXmlTextReaderImpl.ParseText(out AStartPosition, AEndPosition: Integer; var AOutOrChars: Integer): Boolean;
label
  LblReadData, ReturnPartialValue;
var
  AChars: TCharArray;
  APosition, ARcount, ARpos, AOrChars, ACharRefEndPos, ACharCount, AOffset: Integer;
  AEntityType: TEntityType;
  C, ACh: Char;
begin
  AChars := FParsingState.Chars;
  APosition := FParsingState.CharPos;
  ARcount := 0;
  ARpos := -1;
  AOrChars := AOutOrChars;

  while True do
  begin

    repeat
      C := AChars[APosition];
      if TdxXmlCharType.CharProperties[C] and TdxXmlCharType.Text = 0 then
        Break;
      AOrChars := AOrChars or Ord(C);
      Inc(APosition);
    until False;

    case C of
      #$0009:
        begin
          Inc(APosition);
          Continue;
        end;
      #$000A:
        begin
          Inc(APosition);
          OnNewLine(APosition);
          Continue;
        end;
      #$000D:
        begin
          if AChars[APosition + 1] = #$000A then
          begin
            if not FParsingState.EolNormalized and (FParsingMode = TParsingMode.Full) then
            begin
              if APosition - FParsingState.CharPos > 0 then
              begin
                if ARcount = 0 then
                begin
                  ARcount := 1;
                  ARpos := APosition;
                end
                else
                begin
                  ShiftBuffer(ARpos + ARcount, ARpos, APosition - ARpos - ARcount);
                  ARpos := APosition - ARcount;
                  Inc(ARcount);
                end;
              end
              else
                Inc(FParsingState.CharPos);
            end;
            Inc(APosition, 2);
          end
          else
            if (APosition + 1 < FParsingState.CharsUsed) or (FParsingState.IsEof) then
            begin
              if not FParsingState.EolNormalized then
                AChars[APosition] := #$000A;
              Inc(APosition);
            end
            else
              goto LblReadData;
          OnNewLine(APosition);
          Continue;
        end;
      '<':
        goto ReturnPartialValue;
      '&':
        begin
          ACharRefEndPos := ParseCharRefInline(APosition, ACharCount, AEntityType);
          if ACharRefEndPos > 0 then
          begin
            if ARcount > 0 then
              ShiftBuffer(ARpos + ARcount, ARpos, APosition - ARpos - ARcount);

            ARpos := APosition - ARcount;
            Inc(ARcount, ACharRefEndPos - APosition - ACharCount);
            APosition := ACharRefEndPos;

            if not TdxXmlCharType.IsWhiteSpace(AChars[ACharRefEndPos - ACharCount]) then
              AOrChars := AOrChars or $FF;
          end
          else
          begin
            if APosition > FParsingState.CharPos then
              goto ReturnPartialValue;
            case HandleEntityReference(False, TEntityExpandType.All, APosition) of
              TEntityType.Unexpanded:
                Throw(SDTDNotImplemented);
              TEntityType.CharacterDec,
              TEntityType.CharacterHex,
              TEntityType.CharacterNamed:
                begin
                  if not TdxXmlCharType.IsWhiteSpace(FParsingState.Chars[APosition - 1]) then
                    AOrChars := AOrChars or $FF;
                end;
              else
                APosition := FParsingState.CharPos;
            end;
            AChars := FParsingState.Chars;
          end;
          Continue;
        end;
      ']':
        begin
          if (FParsingState.CharsUsed - APosition < 3) and not FParsingState.IsEof then
            goto LblReadData;

          if (AChars[APosition + 1] = ']') and (AChars[APosition + 2] = '>') then
            Throw(APosition, SXmlCDATAEndInText);
          AOrChars := AOrChars or Ord(']');
          Inc(APosition);
          Continue;
        end;
      else
      begin
        if APosition = FParsingState.CharsUsed then
          goto LblReadData
        else
        begin
          ACh := AChars[APosition];
          if TdxXmlCharType.IsHighSurrogate(ACh) then
          begin
            if APosition + 1 = FParsingState.CharsUsed then
              goto LblReadData;
            Inc(APosition);
            if TdxXmlCharType.IsLowSurrogate(AChars[APosition]) then
            begin
              Inc(APosition);
              AOrChars := AOrChars or Ord(ACh);
              Continue;
            end;
          end;
          AOffset := APosition - FParsingState.CharPos;
            ThrowInvalidChar(FParsingState.Chars, FParsingState.CharsUsed, FParsingState.CharPos + AOffset);
          Break;
        end;
      end;
    end;

LblReadData:
    if APosition > FParsingState.CharPos then
      goto ReturnPartialValue;

    if ReadData = 0 then
    begin
      if FParsingState.CharsUsed - FParsingState.CharPos > 0 then
      begin
        if (FParsingState.Chars[FParsingState.CharPos] <> Char($D)) and (FParsingState.Chars[FParsingState.CharPos] <> ']') then
          Throw(SXmlUnexpectedEOF1);
        Assert(FParsingState.IsEof);
      end
      else
      begin
        Break;
      end;
    end;
    APosition := FParsingState.CharPos;
    AChars := FParsingState.Chars;
  end;
  AStartPosition := APosition;
  AEndPosition := APosition;
  Exit(True);

ReturnPartialValue:
  if (FParsingMode = TParsingMode.Full) and (ARcount > 0) then
    ShiftBuffer(ARpos + ARcount, ARpos, APosition - ARpos - ARcount);

  AStartPosition := FParsingState.CharPos;
  AEndPosition := APosition - ARcount;
  FParsingState.CharPos := APosition;
  AOutOrChars := AOrChars;
  Result := C = '<';
end;

function TdxXmlTextReaderImpl.ParseUnexpectedToken(APosition: Integer): string;
begin
  FParsingState.CharPos := APosition;
  Result := ParseUnexpectedToken;
end;

function TdxXmlTextReaderImpl.ParseUnexpectedToken: string;
var
  APosition: Integer;
begin
  if FParsingState.CharPos = FParsingState.CharsUsed then
    Exit('');
  if TdxXmlCharType.IsNCNameSingleChar(FParsingState.Chars[FParsingState.CharPos]) then
  begin
    APosition := FParsingState.CharPos + 1;
    while TdxXmlCharType.IsNCNameSingleChar(FParsingState.Chars[APosition]) do
      Inc(APosition);
    SetString(Result, PChar(@FParsingState.Chars[FParsingState.CharPos]), APosition - FParsingState.CharPos);
  end
  else
  begin
    Assert(FParsingState.CharPos < FParsingState.CharsUsed);
    SetString(Result, PChar(@FParsingState.Chars[FParsingState.CharPos]), 1);
  end;
end;

{ TdxXmlTextReaderImpl }

function TdxXmlTextReaderImpl.ParseXmlDeclaration(AIsTextDecl: Boolean): Boolean;

  procedure ThrowTextDecl;
  begin
    Throw(IfThen(AIsTextDecl, SXmlInvalidTextDecl, SXmlInvalidXmlDecl));
  end;

label
  NoXmlDecl, LblReadData, LblContinue;
var
  ASb: TStringBuilder;
  AXmlDeclState, AOriginalSbLen, AWsCount, ANameEndPos, APosition: Integer;
  AEncoding: TEncoding;
  AEncodingName: string;
  ABadVersion: string;
  AAttr: TdxNodeData;
  AQuoteChar: Char;
  AChars: TCharArray;
begin
  while FParsingState.CharsUsed - FParsingState.CharPos < 6 do
    if ReadData = 0 then
      goto NoXmlDecl;

  if (not StrEqual(FParsingState.Chars, FParsingState.CharPos, 5, XmlDeclarationBeginning)) or
    (TdxXmlCharType.IsNameSingleChar(FParsingState.Chars[FParsingState.CharPos + 5])) then
    goto NoXmlDecl;

  if not AIsTextDecl then
  begin
    FCurrentNode.SetLineInfo(FParsingState.LineNo, FParsingState.LinePos + 2);
    FCurrentNode.SetNamedNode(TdxXmlNodeType.XmlDeclaration, Xml);
  end;
  Inc(FParsingState.CharPos, 5);

  Assert((FStringBuilder.Length = 0) or AIsTextDecl);
  if AIsTextDecl then
    ASb := TStringBuilder.Create
  else
    ASb := FStringBuilder;

  AXmlDeclState := 0;
  AEncoding := nil;

  while True do
  begin
    AOriginalSbLen := ASb.Length;
    if AXmlDeclState = 0 then
      AWsCount := EatWhitespaces(nil)
    else
      AWsCount := EatWhitespaces(ASb);

    if FParsingState.Chars[FParsingState.CharPos] = '?' then
    begin
      ASb.Length := AOriginalSbLen;

      if FParsingState.Chars[FParsingState.CharPos + 1] = '>' then
      begin
        if AXmlDeclState = 0 then
          ThrowTextDecl;

        Inc(FParsingState.CharPos, 2);
        if not AIsTextDecl then
        begin
          FCurrentNode.SetValue(ASb.ToString);
          ASb.Length := 0;

          FNextParsingFunction := FParsingFunction;
          FParsingFunction := TParsingFunction.ResetAttributesRootLevel;
        end;

        if AEncoding = nil then
        begin
          if AIsTextDecl then
            Throw(SXmlInvalidTextDecl);

          if FAfterResetState then
          begin
            AEncodingName := FParsingState.Encoding.WebName;
            if (((AEncodingName <> 'utf-8') and (AEncodingName <> 'utf-16')) and (AEncodingName <> 'utf-16be'))
              and not (FParsingState.Encoding is TMBCSEncoding) then
              Throw(SXmlEncodingSwitchAfterResetState, IfThen(FParsingState.Encoding.GetByteCount('A') = 1, 'UTF-8', 'UTF-16'));
          end;
          if FParsingState.Decoder = TEncoding.ASCII then
            SwitchEncodingToUTF8;
        end
        else
          SwitchEncoding(AEncoding);
        FParsingState.AppendMode := False;
        Exit(True);
      end
      else
        if FParsingState.CharPos + 1 = FParsingState.CharsUsed then
          goto LblReadData
        else
          ThrowUnexpectedToken(#$27'>'#$27);
    end;

    if (AWsCount = 0) and (AXmlDeclState <> 0) then
      ThrowUnexpectedToken('?>');

    ANameEndPos := ParseName;

    AAttr := nil;
    case FParsingState.Chars[FParsingState.CharPos] of
      'v':
        begin
          if StrEqual(FParsingState.Chars, FParsingState.CharPos, ANameEndPos - FParsingState.CharPos, 'version') and (AXmlDeclState = 0) then
          begin
            if not AIsTextDecl then
              AAttr := AddAttributeNoChecks('version', 1);
          end
          else
            ThrowTextDecl;
        end;
      'e':
        begin
          if StrEqual(FParsingState.Chars, FParsingState.CharPos, ANameEndPos - FParsingState.CharPos, 'encoding') and ((AXmlDeclState = 1) or ((AIsTextDecl and (AXmlDeclState = 0)))) then
          begin
            if not AIsTextDecl then
              AAttr := AddAttributeNoChecks('encoding', 1);
            AXmlDeclState := 1;
          end
          else
            ThrowTextDecl;
        end;
      's':
        begin
          if StrEqual(FParsingState.Chars, FParsingState.CharPos, ANameEndPos - FParsingState.CharPos, 'standalone') and ((AXmlDeclState = 1) or (AXmlDeclState = 2)) and not AIsTextDecl then
          begin
            if not AIsTextDecl then
              AAttr := AddAttributeNoChecks('standalone', 1);
            AXmlDeclState := 2;
          end
          else
            ThrowTextDecl;
        end;
      else
        ThrowTextDecl;
    end;
    if not AIsTextDecl then
      AAttr.SetLineInfo(FParsingState.LineNo, FParsingState.LinePos);
    ASb.Append(FParsingState.Chars, FParsingState.CharPos, ANameEndPos - FParsingState.CharPos);
    FParsingState.CharPos := ANameEndPos;

    if FParsingState.Chars[FParsingState.CharPos] <> '=' then
    begin
      EatWhitespaces(ASb);
      if FParsingState.Chars[FParsingState.CharPos] <> '=' then
        ThrowUnexpectedToken('=');
    end;
    ASb.Append('=');
    Inc(FParsingState.CharPos);

    AQuoteChar := FParsingState.Chars[FParsingState.CharPos];
    if (AQuoteChar <> '"') and (AQuoteChar <> #$27) then
    begin
      EatWhitespaces(ASb);
      AQuoteChar := FParsingState.Chars[FParsingState.CharPos];
      if (AQuoteChar <> '"') and (AQuoteChar <> #$27) then
        ThrowUnexpectedToken('"', #$27);
    end;
    ASb.Append(AQuoteChar);
    Inc(FParsingState.CharPos);
    if not AIsTextDecl then
    begin
      AAttr.QuoteChar := AQuoteChar;
      AAttr.SetLineInfo2(FParsingState.LineNo, FParsingState.LinePos);
    end;
    APosition := FParsingState.CharPos;

LblContinue:
    AChars := FParsingState.Chars;

    while ((TdxXmlCharType.CharProperties[AChars[APosition]] and TdxXmlCharType.AttrValue) <> 0) do
      Inc(APosition);

    if FParsingState.Chars[APosition] = AQuoteChar then
    begin
      case AXmlDeclState of
        0:
          if StrEqual(FParsingState.Chars, FParsingState.CharPos, APosition - FParsingState.CharPos, '1.0') then
          begin
            if not AIsTextDecl then
              AAttr.SetValue(FParsingState.Chars, FParsingState.CharPos, APosition - FParsingState.CharPos);
            AXmlDeclState := 1;
          end
          else
          begin
            SetString(ABadVersion, PChar(@FParsingState.Chars[FParsingState.CharPos]), APosition - FParsingState.CharPos);
            Throw(SXmlInvalidVersionNumber, ABadVersion);
          end;
        1:
          begin
            SetString(AEncodingName, PChar(@FParsingState.Chars[FParsingState.CharPos]), APosition - FParsingState.CharPos);
            AEncoding := CheckEncoding(AEncodingName);
            if not AIsTextDecl then
              AAttr.SetValue(AEncodingName);
            AXmlDeclState := 2;
          end;
        2:
          begin
            if StrEqual(FParsingState.Chars, FParsingState.CharPos, APosition - FParsingState.CharPos, 'yes') then
              FStandalone := True
            else
              if StrEqual(FParsingState.Chars, FParsingState.CharPos, APosition - FParsingState.CharPos, 'no') then
                FStandalone := False
              else
              begin
                Assert(not AIsTextDecl);
                Throw(SXmlInvalidXmlDecl, FParsingState.LineNo, FParsingState.LinePos - 1);
              end;
            if not AIsTextDecl then
              AAttr.SetValue(FParsingState.Chars, FParsingState.CharPos, APosition - FParsingState.CharPos);
            AXmlDeclState := 3;
          end;
        else
          Assert(False);
      end;
      ASb.Append(AChars, FParsingState.CharPos, APosition - FParsingState.CharPos);
      ASb.Append(AQuoteChar);
      FParsingState.CharPos := APosition + 1;
      Continue;
    end
    else
      if APosition = FParsingState.CharsUsed then
      begin
        if ReadData <> 0 then
          goto LblContinue
        else
          Throw(SXmlUnclosedQuote);
      end
      else
        ThrowTextDecl;

LblReadData:
    if (FParsingState.IsEof) or (ReadData = 0) then
      Throw(SXmlUnexpectedEOF1);
  end;

NoXmlDecl:
  if not AIsTextDecl then
    FParsingFunction := FNextParsingFunction;

  if FAfterResetState then
  begin
    AEncodingName := FParsingState.Encoding.WebName;
    if (AEncodingName <> 'utf-8') and (AEncodingName <> 'utf-16') and (AEncodingName <> 'utf-16be') and
       not (FParsingState.Encoding is TMBCSEncoding) then
      Throw(SXmlEncodingSwitchAfterResetState, IfThen(FParsingState.Encoding.GetByteCount('A') = 1, 'UTF-8', 'UTF-16'));
  end;

  if FParsingState.Decoder = TEncoding.ASCII then
    SwitchEncodingToUTF8;
  FParsingState.AppendMode := False;
  Result := False;
end;

procedure TdxXmlTextReaderImpl.ParseXmlDeclarationFragment;
begin
  try
    ParseXmlDeclaration(False);
  except
    on E: EdxXmlException do
      ReThrow(E, E.LineNumber, E.LinePosition - 6);
  end;
end;

procedure TdxXmlTextReaderImpl.SkipPartialTextValue;
var
  AStartPos, AEndPos, AOrChars: Integer;
begin
  Assert(FParsingFunction in [
    TParsingFunction.PartialTextValue,
    TParsingFunction.InReadValueChunk,
    TParsingFunction.InReadContentAsBinary,
    TParsingFunction.InReadElementContentAsBinary]);

  AOrChars := 0;
  FParsingFunction := FNextParsingFunction;
  while not ParseText(AStartPos, AEndPos, AOrChars) do ;
end;

procedure TdxXmlTextReaderImpl.ReThrow(E: Exception; ALineNo, ALinePos: Integer);
begin
  Throw(EdxXmlException.Create(E.Message, nil, ALineNo, ALinePos, ''));
end;

function TdxXmlTextReaderImpl.Read: Boolean;
begin
  if FLaterInitParam <> nil then
    FinishInit;

  while True do
  begin
    case FParsingFunction of
      TParsingFunction.ElementContent:
        Exit(ParseElementContent);
      TParsingFunction.DocumentContent:
        Exit(ParseDocumentContent);
      TParsingFunction.SwitchToInteractive:
        begin
          Assert(not FParsingState.AppendMode);
          FReadState := TdxXmlReadState.Interactive;
          FParsingFunction := FNextParsingFunction;
          Continue;
        end;
      TParsingFunction.SwitchToInteractiveXmlDecl:
        begin
          FReadState := TdxXmlReadState.Interactive;
          FParsingFunction := FNextParsingFunction;
          if ParseXmlDeclaration(False) then
            Exit(True);
          Continue;
        end;
      TParsingFunction.ResetAttributesRootLevel:
        begin
          ResetAttributes;
          FCurrentNode := FNodes[FIndex];
          if (FIndex = 0) then
            FParsingFunction := TParsingFunction.DocumentContent
          else
            FParsingFunction := TParsingFunction.ElementContent;
          Continue;
        end;
      TParsingFunction.MoveToElementContent:
        begin
          ResetAttributes;
          Inc(FIndex);
          FCurrentNode := AddNode(FIndex, FIndex);
          FParsingFunction := TParsingFunction.ElementContent;
          Continue;
        end;
      TParsingFunction.PopElementContext:
        begin
          PopElementContext;
          FParsingFunction := FNextParsingFunction;
          Assert(FParsingFunction in [TParsingFunction.ElementContent, TParsingFunction.DocumentContent]);
          Continue;
        end;
      TParsingFunction.PopEmptyElementContext:
        begin
          FCurrentNode := FNodes[FIndex];
          Assert(FCurrentNode.&Type = TdxXmlNodeType.Element);
          FCurrentNode.IsEmptyElement := False;
          ResetAttributes;
          PopElementContext;
          FParsingFunction := FNextParsingFunction;
          Continue;
        end;
      TParsingFunction.XmlDeclarationFragment:
        begin
          ParseXmlDeclarationFragment;
          FParsingFunction := TParsingFunction.GoToEof;
          Exit(True);
        end;
      TParsingFunction.GoToEof:
        begin
          OnEof;
          Exit(False);
        end;
      TParsingFunction.Error,
      TParsingFunction.Eof,
      TParsingFunction.ReaderClosed:
        Exit(False);
      TParsingFunction.NoData:
        begin
          ThrowWithoutLineInfo(SXmlMissingRoot);
          Exit(False);
        end;
      TParsingFunction.PartialTextValue:
        begin
          SkipPartialTextValue;
          Continue;
        end;
      else
        Assert(False);
    end;
  end;
end;

function TdxXmlTextReaderImpl.ReadData: Integer;
var
  ACharsRead, I, ACopyCharsCount, ABytesLeft, ARead, AOriginalBytePos, ACharsLen: Integer;
  ABytesRead: Integer;
begin
  if FParsingState.IsEof then
    Exit(0);

  if FParsingState.AppendMode then
  begin
    if FParsingState.CharsUsed = Length(FParsingState.Chars) - 1 then
    begin
      for I := 0 to FAttributeCount - 1 do
        FNodes[FIndex + I + 1].OnBufferInvalidated;
      SetLength(FParsingState.Chars, Length(FParsingState.Chars) * 2);
    end;

    if FParsingState.Stream <> nil then
      if FParsingState.BytesUsed - FParsingState.BytePos < MaxByteSequenceLen then
        if Length(FParsingState.Bytes) - FParsingState.BytesUsed < MaxByteSequenceLen then
          SetLength(FParsingState.Bytes, Length(FParsingState.Bytes) * 2);

    ACharsRead := Length(FParsingState.Chars) - FParsingState.CharsUsed - 1;
    if ACharsRead > ApproxXmlDeclLength then
      ACharsRead := ApproxXmlDeclLength;
  end
  else
  begin
    ACharsLen := Length(FParsingState.Chars);
    if ACharsLen - FParsingState.CharsUsed <= ACharsLen div 2 then
    begin
      for I := 0 to FAttributeCount - 1 do
        FNodes[FIndex + I + 1].OnBufferInvalidated;
      ACopyCharsCount := FParsingState.CharsUsed - FParsingState.CharPos;
      if ACopyCharsCount < ACharsLen - 1 then
      begin
        FParsingState.LineStartPos := FParsingState.LineStartPos - FParsingState.CharPos;
        if ACopyCharsCount > 0 then
          BlockCopyChars(FParsingState.Chars, FParsingState.CharPos, FParsingState.Chars, 0, ACopyCharsCount);
        FParsingState.CharPos := 0;
        FParsingState.CharsUsed := ACopyCharsCount;
      end
      else
        SetLength(FParsingState.Chars, Length(FParsingState.Chars) * 2);
    end;

    if FParsingState.Stream <> nil then
    begin
      ABytesLeft := FParsingState.BytesUsed - FParsingState.BytePos;
      if ABytesLeft <= MaxBytesToMove then
      begin
        if ABytesLeft = 0 then
          FParsingState.BytesUsed := 0
        else
        begin
          Move(FParsingState.Bytes[FParsingState.BytePos], FParsingState.Bytes[0], ABytesLeft);
          FParsingState.BytesUsed := ABytesLeft;
        end;
        FParsingState.BytePos := 0;
      end;
    end;
    ACharsRead := Length(FParsingState.Chars) - FParsingState.CharsUsed - 1;
  end;

  if FParsingState.Stream <> nil then
  begin
    if not FParsingState.IsStreamEof then
    begin
      ABytesRead := Length(FParsingState.Bytes) - FParsingState.BytesUsed;
      if ABytesRead > 0 then
      begin
        ARead := FParsingState.Stream.Read(FParsingState.Bytes[FParsingState.BytesUsed], ABytesRead);

        if ARead = 0 then
          FParsingState.IsStreamEof := True;
        Inc(FParsingState.BytesUsed, ARead);
      end;
    end;

    AOriginalBytePos := FParsingState.BytePos;

    ACharsRead := GetChars(ACharsRead);
    if (ACharsRead = 0) and (FParsingState.BytePos <> AOriginalBytePos) then
      Exit(ReadData);
  end
  else
    ACharsRead := 0;

  RegisterConsumedCharacters(ACharsRead);

  if ACharsRead = 0 then
  begin
    Assert(FParsingState.CharsUsed < Length(FParsingState.Chars));
    FParsingState.IsEof := True;
  end;
  FParsingState.Chars[FParsingState.CharsUsed] := #$0000;
  Result := ACharsRead;
end;

procedure TdxXmlTextReaderImpl.RegisterConsumedCharacters(ACharacters: Int64);
var
  ANewCharactersInDocument: Int64;
begin
  Assert(ACharacters >= 0);
  if FMaxCharactersInDocument > 0 then
  begin
    ANewCharactersInDocument := FCharactersInDocument + ACharacters;
    if ANewCharactersInDocument < FCharactersInDocument then
      ThrowWithoutLineInfo(SXmlLimitExceeded, 'MaxCharactersInDocument')
    else
      FCharactersInDocument := ANewCharactersInDocument;
    if FCharactersInDocument > FMaxCharactersInDocument then
      ThrowWithoutLineInfo(SXmlLimitExceeded, 'MaxCharactersInDocument');
  end;
end;

procedure TdxXmlTextReaderImpl.ResetAttributes;
begin
  if FFullAttributeCleanup then
    FullAttributeCleanup;
  FCurrentAttributeIndex := -1;
  FAttributeCount := 0;
  FAttributeHashTable := 0;
  FAttributeDuplicateWalkCount := 0;
end;

procedure TdxXmlTextReaderImpl.SetErrorState;
begin
  FParsingFunction := TParsingFunction.Error;
  FReadState := TdxXmlReadState.Error;
end;

procedure TdxXmlTextReaderImpl.SetupEncoding(AEncoding: TEncoding);
begin
  if AEncoding = nil then
  begin
    Assert(FParsingState.CharPos = 0);
    FParsingState.Encoding := TEncoding.UTF8;
    FParsingState.Decoder := TEncoding.ASCII;
  end
  else
  begin
    FParsingState.Encoding := AEncoding;
    if ContainsStr(FParsingState.Encoding.WebName, 'utf-16') then
      FParsingState.Decoder := TEncoding.Unicode
    else
      FParsingState.Decoder := AEncoding;
  end;
end;

procedure TdxXmlTextReaderImpl.SwitchEncoding(ANewEncoding: TEncoding);
begin
  if (ANewEncoding.WebName <> FParsingState.Encoding.WebName) or (FParsingState.Decoder = TEncoding.ASCII)
    and not FAfterResetState then
  begin
    Assert(FParsingState.Stream <> nil);
    UnDecodeChars;
    FParsingState.AppendMode := False;
    SetupEncoding(ANewEncoding);
    ReadData;
  end;
end;

procedure TdxXmlTextReaderImpl.SwitchEncodingToUTF8;
begin
  SwitchEncoding(TEncoding.UTF8);
end;

procedure TdxXmlTextReaderImpl.ThrowUnexpectedToken(APosition: Integer; const AExpectedToken1, AExpectedToken2: string);
begin
  FParsingState.CharPos := APosition;
  ThrowUnexpectedToken(AExpectedToken1, AExpectedToken2);
end;

procedure TdxXmlTextReaderImpl.Throw(const ARes: string);
begin
  Throw(ARes, '');
end;

procedure TdxXmlTextReaderImpl.Throw(APosition: Integer; const ARes: string);
begin
  FParsingState.CharPos := APosition;
  Throw(ARes, '');
end;

procedure TdxXmlTextReaderImpl.Throw(const ARes: string; ALineNo, ALinePos: Integer);
begin
  Throw(EdxXmlException.Create(ARes, '', ALineNo, ALinePos, ''));
end;

procedure TdxXmlTextReaderImpl.Throw(const ARes, AArg: string);
begin
  Throw(EdxXmlException.Create(ARes, AArg, FParsingState.LineNo, FParsingState.LinePos, ''));
end;

procedure TdxXmlTextReaderImpl.ThrowUnclosedElements;
var
  I: Integer;
  AElement: TdxNodeData;
begin
  if (FIndex = 0) and (FCurrentNode.&Type <> TdxXmlNodeType.Element) then
    Throw(FParsingState.CharsUsed, SXmlUnexpectedEOF1)
  else
  begin
    if (FParsingFunction = TParsingFunction.InIncrementalRead) then
      I := FIndex
    else
      I := FIndex - 1;
    FStringBuilder.Length := 0;
    while I >= 0 do
    begin
      AElement := FNodes[I];
      if AElement.&Type <> TdxXmlNodeType.Element then
        Continue;
      FStringBuilder.Append(AElement.GetNameWPrefix(FNameTable));
      if I > 0 then
        FStringBuilder.Append(', ')
      else
        FStringBuilder.Append('.');
      Dec(I);
    end;
    Throw(FParsingState.CharsUsed, SXmlUnexpectedEOFInElementContent, FStringBuilder.ToString);
  end;
end;

procedure TdxXmlTextReaderImpl.ThrowUnexpectedToken(const AExpectedToken1, AExpectedToken2: string);
var
  AUnexpectedToken: string;
begin
  AUnexpectedToken := ParseUnexpectedToken;
  if AUnexpectedToken = '' then
    Throw(SXmlUnexpectedEOF1);
  if AExpectedToken2 <> '' then
    Throw(SXmlUnexpectedTokens2, [AUnexpectedToken, AExpectedToken1, AExpectedToken2])
  else
    Throw(SXmlUnexpectedTokenEx, [AUnexpectedToken, AExpectedToken1]);
end;

procedure TdxXmlTextReaderImpl.ThrowUnexpectedToken(AExpectedToken: string);
begin
  ThrowUnexpectedToken(AExpectedToken, '');
end;

procedure TdxXmlTextReaderImpl.ThrowWithoutLineInfo(const ARes, AArg: string);
begin
  Throw(EdxXmlException.Create(ARes, AArg, ''));
end;

class function TdxXmlTextReaderImpl.ConvertToConstArray(const AArgs: TArray<string>): TArray<TVarRec>;
var
  I: Integer;
begin
  SetLength(Result, Length(AArgs));
  for I := Low(AArgs) to High(AArgs) do
  begin
    string(Result[I].VUnicodeString) := UnicodeString(AArgs[I]);
    Result[I].VType := vtUnicodeString;
  end;
end;

procedure TdxXmlTextReaderImpl.ThrowWithoutLineInfo(const ARes: string);
begin
  Throw(EdxXmlException.Create(ARes, '', ''));
end;

procedure TdxXmlTextReaderImpl.UnDecodeChars;
begin
  Assert(((FParsingState.Stream <> nil) and (FParsingState.Decoder <> nil)) and (FParsingState.Bytes <> nil));
  Assert(FParsingState.AppendMode, 'UnDecodeChars cannot be called after ps.appendMode has been changed to false');

  Assert(FParsingState.CharsUsed >= FParsingState.CharPos, 'The current position must be in the valid character range.');
  if FMaxCharactersInDocument > 0 then
  begin
    Assert(FCharactersInDocument >= FParsingState.CharsUsed - FParsingState.CharPos,
      'We didn'#$27't correctly count some of the decoded characters against the MaxCharactersInDocument.');
    FCharactersInDocument := FCharactersInDocument - FParsingState.CharsUsed - FParsingState.CharPos;
  end;
  FParsingState.BytePos := FDocumentStartBytePos;
  if FParsingState.CharPos > 0 then
    Inc(FParsingState.BytePos, FParsingState.Encoding.GetByteCount(FParsingState.Chars, 0, FParsingState.CharPos));
  FParsingState.CharsUsed := FParsingState.CharPos;
  FParsingState.IsEof := False;
end;

{ TdxXmlTextReaderImpl.TdxXmlContext }

constructor TdxXmlTextReaderImpl.TXmlContext.Create(APreviousContext: TXmlContext);
begin
  inherited Create;
  XmlSpace := APreviousContext.XmlSpace;
  XmlLang := APreviousContext.XmlLang;
  DefaultNamespace := APreviousContext.DefaultNamespace;
  PreviousContext := APreviousContext;
end;

procedure TdxXmlTextReaderImpl.Throw(APosition: Integer; const ARes, AArg: string);
begin
  FParsingState.CharPos := APosition;
  Throw(ARes, AArg);
end;

procedure TdxXmlTextReaderImpl.Throw(E: Exception);
var
  AXmlEx: EdxXmlException;
begin
  SetErrorState;
  AXmlEx := E as EdxXmlException;
  if AXmlEx <> nil then
    FCurrentNode.SetLineInfo(AXmlEx.LineNumber, AXmlEx.LinePosition);
  raise E;
end;

procedure TdxXmlTextReaderImpl.ThrowExpectingWhitespace(APosition: Integer);
var
  AUnexpectedToken: string;
begin
  AUnexpectedToken := ParseUnexpectedToken(APosition);
  if AUnexpectedToken = '' then
    Throw(APosition, SXmlUnexpectedEOF1)
  else
    Throw(APosition, SXmlExpectingWhiteSpace, AUnexpectedToken);
end;

procedure TdxXmlTextReaderImpl.ThrowInvalidChar(const AData: TCharArray; ALength, AInvCharPos: Integer);
begin
  Throw(AInvCharPos, SXmlInvalidCharacter, EdxXmlException.BuildCharExceptionArgs(AData, ALength, AInvCharPos));
end;

procedure TdxXmlTextReaderImpl.Throw(const ARes: string; const AArgs: array of const);
begin
  Throw(EdxXmlException.Create(ARes, AArgs, FParsingState.LineNo, FParsingState.LinePos, ''));
end;

procedure TdxXmlTextReaderImpl.ThrowTagMismatch(AStartTag: TdxNodeData);
var
  AColonPos, AEndPos: Integer;
  AArg0, AArg1, AArg2, AArg3: string;
begin
  if AStartTag.&Type = TdxXmlNodeType.Element then
  begin
    AEndPos := ParseQName(AColonPos);
    AArg0 := AStartTag.GetNameWPrefix(FNameTable);
    AArg1 := IntToStr(AStartTag.LineInfo.LineNo);
    AArg2 := IntToStr(AStartTag.LineInfo.LinePos);
    SetString(AArg3, PChar(@FParsingState.Chars[FParsingState.CharPos]), AEndPos - FParsingState.CharPos);
    Throw(SXmlTagMismatchEx, [AArg0, AArg1, AArg2, AArg3]);
  end
  else
  begin
    Assert(AStartTag.&Type = TdxXmlNodeType.EntityReference);
    Throw(SXmlUnexpectedEndTag);
  end;
end;

procedure TdxXmlTextReaderImpl.Throw(const ARes, AArg: string; ALineNo, ALinePos: Integer);
begin
  Throw(EdxXmlException.Create(ARes, AArg, ALineNo, ALinePos, ''));
end;

procedure TdxXmlTextReaderImpl.Throw(APosition: Integer; const ARes: string; const AArgs: array of const);
begin
  FParsingState.CharPos := APosition;
  Throw(ARes, AArgs);
end;

procedure TdxXmlTextReaderImpl.Throw(APosition: Integer; const ARes: string; const AArgs: TArray<string>);
begin
  Throw(APosition, ARes, ConvertToConstArray(AArgs));
end;

{ TdxXmlNameTable }

constructor TdxXmlNameTable.Create;
begin
  inherited Create;
  FEntries := TdxStringSet.Create;
end;

destructor TdxXmlNameTable.Destroy;
begin
  FEntries.Free;
  inherited Destroy;
end;

function TdxXmlNameTable.Add(const AKey: string): string;
begin
  Result := FEntries.Add(AKey);
end;

function TdxXmlNameTable.Add(const AKey: TCharArray; AStart, ALen: Integer): string;
var
  S: string;
begin
  SetString(S, PChar(@AKey[AStart]), ALen);
  Result := FEntries.Add(S);
end;

function TdxXmlNameTable.Get(const AValue: string): string;
begin
  if FEntries.Contains(AValue) then
    Result := AValue
  else
    Result := '';
end;

function TdxXmlNameTable.Get(const AKey: TCharArray; AStart, ALen: Integer): string;
var
  AValue: string;
begin
  SetString(AValue, PChar(@AKey[AStart]), ALen);
  Result := Get(AValue);
end;

{ TdxXmlTextReaderImpl.TParsingState }

procedure TdxXmlTextReaderImpl.TParsingState.Clear;
begin
  Chars := nil;
  CharPos := 0;
  CharsUsed := 0;
  Encoding := nil;
  Stream := nil;
  Decoder := nil;
  Bytes := nil;
  BytePos := 0;
  BytesUsed := 0;
  LineNo := 1;
  LineStartPos := -1;
  IsEof := False;
  IsStreamEof := False;
  EolNormalized := True;
end;

function TdxXmlTextReaderImpl.TParsingState.GetLinePos: Integer;
begin
  Result := CharPos - LineStartPos;
end;

{$ENDIF}
end.
