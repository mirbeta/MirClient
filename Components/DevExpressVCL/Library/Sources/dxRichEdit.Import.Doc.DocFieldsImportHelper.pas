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
unit dxRichEdit.Import.Doc.DocFieldsImportHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics,

  dxRichEdit.Utils.Token,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.ListFormatInformation;

type

  TdxFieldType = (
    None,
    CreateDate,
    Date,
    EditTime,
    PrintDate,
    SaveDate,
    Time,
    Compare,
    DocVariable,
    GotoButton,
    &If,
    MacroButton,
    Print,
    Author,
    Comments,
    DocProperty,
    FileName,
    FileSize,
    Info,
    Keywords,
    LastSavedBy,
    NumChars,
    NumPages,
    NumWords,
    Subject,
    Template,
    Title,
    Formula,
    Advance,
    Eq,
    Symbol,
    Index,
    RD,
    TA,
    TC,
    TOA,
    TOC,
    XE,
    AutoText,
    AutoTextList,
    Bibliography,
    Citation,
    Hyperlink,
    IncludePicture,
    IncludeText,
    Link,
    NoteRef,
    PageRef,
    Quote,
    Ref,
    StyleRef,
    AddressBlock,
    Ask,
    DataBase,
    FillIn,
    GreetingLine,
    MergeField,
    MergeRec,
    MergeSeq,
    Next,
    NextIf,
    &Set,
    SkipIf,
    AutoNum,
    AutoNumLGL,
    AutoNumOut,
    BarCode,
    ListNum,
    Page,
    RevNum,
    Section,
    SectionPages,
    Seq,
    UserAddress,
    UserInitials,
    UserName,
    FormCheckbox,
    FormDropdown,
    FormText
  );

  { TdxIDocFieldDescriptor }

  IdxDocFieldDescriptor = interface
  ['{4CF1C017-F104-44ED-8205-49D5169CF99E}']
    function GetBoundaryType: Char;
    procedure Write(AWriter: TBinaryWriter);

    property BoundaryType: Char read GetBoundaryType;
  end;

  { TdxDocHyperlinkInformationFlags }

  TdxDocHyperlinkInformationFlags = class
  public const
    NewWindow = $01;
    NoHistory = $02;
    ImageMap = $04;
    Location = $08;
    Tooltip = $10;
  end;

  { TdxDocHyperlinkObjectFlags }

  TdxDocHyperlinkObjectFlags = class
  public const
    HasMoniker = $0001;
    IsAbsolute = $0002;
    SiteGaveDisplayName = $0004;
    HasLocationString = $0008;
    HasDisplayName = $0010;
    HasGUID = $0020;
    HasCreationTime = $0040;
    HasFrameName = $0080;
    MonikerSavedAsString = $0100;
    AbsoluteFromRelative = $0200;
  end;

  { TdxDocFieldBinaryData }

  TdxDocFieldBinaryData = class abstract
  public const
    HeaderSize      = $44;
    IgnoredDataSize = $3e;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer);
    procedure Traverse(AReader: TBinaryReader); virtual; abstract;
    procedure WriteBinaryData(AWriter: TBinaryWriter); virtual; abstract;
    function GetSize: Integer; virtual; abstract;
  public
    constructor Create(AReader: TBinaryReader; AOffset: Integer);
    procedure Write(AWriter: TBinaryWriter);
  end;

  { TdxDocHyperlinkInfo }

  TdxDocHyperlinkInfo = class(TdxDocFieldBinaryData)
  public const
    ComIdSize         = 16;
    StreamVersion     = 2;
    StreamVersionSize = 4;
    DelimiterSize     = 2;
    BaseSize          = 25;
  strict private
    FHyperlinkInformationFlags: Byte;
    FHyperlinkObjectFlags: Integer;
    FDisplayName: string;
    FTargetFrameName: string;
  protected
    procedure Traverse(AReader: TBinaryReader); override;
    procedure WriteBinaryData(AWriter: TBinaryWriter); override;
    function GetSize: Integer; override;
    function ReadHyperlinkString(AReader: TBinaryReader): string;
    function CalcHyperlinkStringLength(const AHyperLinkString: string): Integer;
  public
    constructor Create(const ADisplayName: string; const ATargetFrameName: string); overload;

    property HyperlinkInformationFlags: Byte read FHyperlinkInformationFlags;
    property HyperlinkObjectFlags: Integer read FHyperlinkObjectFlags;
  end;

  { TdxFieldStartComparer }

  TdxFieldStartComparer = class(TComparer<TdxField>)
  public
    function Compare(const Left, Right: TdxField): Integer; override;
  end;

  TdxFieldProperties = class
  public const
    Differ = $01;
    ZombieEmbed = $02;
    ResultsDirty = $04;
    ResultsEdited = $08;
    Locked = $10;
    PrivateResult = $20;
    Nested = $40;
    HasSeparator = $80;
  end;

  { TdxDocFieldEndDescriptor }

  TdxDocFieldEndDescriptor = class(TInterfacedObject, IdxDocFieldDescriptor)
  strict private
    FProperties: Byte;
  protected
    function GetBoundaryType: Char;
  public
    constructor Create; overload;
    constructor Create(ABitField: Byte); overload;
    procedure Write(AWriter: TBinaryWriter);

    property BoundaryType: Char read GetBoundaryType;
    property Properties: Byte read FProperties write FProperties;
  end;

  { TdxDocFieldBeginDescriptor }

  TdxDocFieldBeginDescriptor = class(TInterfacedObject, IdxDocFieldDescriptor)
  public const
    UnknownFieldType            = $01;
    RefFieldType                = $03;
    EvaluateExpressionFieldType = $22;
    NoteRefFieldType            = $48;
    HyperlinkFieldType          = $58;
  strict private
    class var
      FFieldTypes: TDictionary<string, Byte>;
      FEmpty: TdxDocFieldBeginDescriptor;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FFieldType: Byte;
    function GetBoundaryType: Char;
  public
    constructor Create(AFieldType: Byte); overload;
    constructor Create(const AToken: IdxToken); overload;
    procedure Write(AWriter: TBinaryWriter);
    function CalcFieldType: TdxFieldType; overload;
    function CalcFieldType(const AVal: string): Byte; overload;

    class property Empty: TdxDocFieldBeginDescriptor read FEmpty;
    property BoundaryType: Char read GetBoundaryType;
  end;

  { TdxDocFieldSeparatorDescriptor }

  TdxDocFieldSeparatorDescriptor = class(TInterfacedObject, IdxDocFieldDescriptor)
  strict private
    function GetBoundaryType: Char;
  public
    procedure Write(AWriter: TBinaryWriter);

    property BoundaryType: Char read GetBoundaryType;
  end;

  { TdxDocFieldDescriptorFactory }

  TdxDocFieldDescriptorFactory = class
  public
    class function CreateInstance(AReader: TBinaryReader): IdxDocFieldDescriptor; static;
  end;

implementation

uses
  Math, Contnrs, dxCharacters,
  dxRichEdit.Import.Rtf,
  dxStringHelper,
  dxRichEdit.Import.Doc.DocImporter,
  dxEncoding;

{ TdxDocFieldBinaryData }

constructor TdxDocFieldBinaryData.Create(AReader: TBinaryReader; AOffset: Integer);
begin
  Assert(AReader <> nil, 'reader');
  Read(AReader, AOffset);
end;

procedure TdxDocFieldBinaryData.Read(AReader: TBinaryReader; AOffset: Integer);
begin
  Assert(AReader <> nil, 'reader');
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  AReader.ReadInt32;
  if AReader.ReadSmallInt <> HeaderSize then
    TdxDocImporter.ThrowInvalidDocFile;

  AReader.BaseStream.Seek(IgnoredDataSize, TSeekOrigin.soCurrent);
  Traverse(AReader);
end;

procedure TdxDocFieldBinaryData.Write(AWriter: TBinaryWriter);
begin
  Assert(AWriter <> nil, 'writer');
  AWriter.Write(GetSize);
  AWriter.BaseStream.Seek(IgnoredDataSize, TSeekOrigin.soCurrent);
  WriteBinaryData(AWriter);
end;

{ TdxDocHyperlinkInfo }

constructor TdxDocHyperlinkInfo.Create(const ADisplayName: string; const ATargetFrameName: string);
begin
  FHyperlinkObjectFlags := TdxDocHyperlinkObjectFlags.HasDisplayName and TdxDocHyperlinkObjectFlags.HasFrameName;
  FDisplayName := ADisplayName;
  FTargetFrameName := ATargetFrameName;
end;

procedure TdxDocHyperlinkInfo.Traverse(AReader: TBinaryReader);
begin
  FHyperlinkInformationFlags := AReader.ReadByte;
  AReader.BaseStream.Seek(ComIdSize, TSeekOrigin.soCurrent);
  AReader.BaseStream.Seek(StreamVersionSize, TSeekOrigin.soCurrent);
  FHyperlinkObjectFlags := AReader.ReadInt32;
  if (FHyperlinkObjectFlags and TdxDocHyperlinkObjectFlags.HasDisplayName) = TdxDocHyperlinkObjectFlags.HasDisplayName then
    FDisplayName := ReadHyperlinkString(AReader);
  if (FHyperlinkObjectFlags and TdxDocHyperlinkObjectFlags.HasFrameName) = TdxDocHyperlinkObjectFlags.HasFrameName then
    FTargetFrameName := ReadHyperlinkString(AReader);
end;

procedure TdxDocHyperlinkInfo.WriteBinaryData(AWriter: TBinaryWriter);
begin
  AWriter.Write(FHyperlinkInformationFlags);
  AWriter.Seek(ComIdSize, TSeekOrigin.soCurrent);
  AWriter.Write(StreamVersion);
  AWriter.Write(FHyperlinkObjectFlags);
  AWriter.Write(FDisplayName);
  AWriter.Write(FTargetFrameName);
end;

function TdxDocHyperlinkInfo.GetSize: Integer;
begin
  Result := BaseSize + CalcHyperlinkStringLength(FDisplayName) + CalcHyperlinkStringLength(FTargetFrameName);
end;

function TdxDocHyperlinkInfo.ReadHyperlinkString(AReader: TBinaryReader): string;
var
  ALength: Integer;
  ABuffer: TBytes;
begin
  ALength := AReader.ReadInt32;
  ABuffer := AReader.ReadBytes(ALength * 2 - DelimiterSize);
  Result := TdxEncoding.Unicode.GetString(ABuffer, 0, Length(ABuffer));
  AReader.ReadSmallInt;
end;

function TdxDocHyperlinkInfo.CalcHyperlinkStringLength(const AHyperLinkString: string): Integer;
begin
  Result := 4 + (Length(AHyperLinkString) * 2) + DelimiterSize;
end;

{ TdxFieldStartComparer }

function TdxFieldStartComparer.Compare(const Left, Right: TdxField): Integer;
begin
  Result := Left.FirstRunIndex - Right.FirstRunIndex;
end;

{ TdxDocFieldEndDescriptor }

constructor TdxDocFieldEndDescriptor.Create(ABitField: Byte);
begin
  FProperties := ABitField;
end;

constructor TdxDocFieldEndDescriptor.Create;
begin
  FProperties := TdxFieldProperties.HasSeparator;
end;

function TdxDocFieldEndDescriptor.GetBoundaryType: Char;
begin
  Result := TdxTextCodes.FieldEnd;
end;

procedure TdxDocFieldEndDescriptor.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(Byte($95));
  AWriter.Write(FProperties);
end;

{ TdxDocFieldBeginDescriptor }

constructor TdxDocFieldBeginDescriptor.Create(const AToken: IdxToken);
begin
  case AToken.ActualKind of
    TdxTokenKind.Eq:
      FFieldType := UnknownFieldType;
    TdxTokenKind.OpEQ:
      FFieldType := EvaluateExpressionFieldType;
    else
      FFieldType := CalcFieldType(AToken.Value);
  end;
end;

constructor TdxDocFieldBeginDescriptor.Create(AFieldType: Byte);
begin
  FFieldType := AFieldType;
end;

class constructor TdxDocFieldBeginDescriptor.Initialize;
begin
  FEmpty := TdxDocFieldBeginDescriptor.Create(UnknownFieldType);
  FFieldTypes := TDictionary<string, Byte>.Create(87);
  FFieldTypes.Add('REF', 3);

  FFieldTypes.Add('FTNREF', 5);
  FFieldTypes.Add('SET', 6);
  FFieldTypes.Add('IF', 7);
  FFieldTypes.Add('INDEX', 8);

  FFieldTypes.Add('STYLEREF', 10);
  FFieldTypes.Add('SEQ', 12);
  FFieldTypes.Add('TOC', 13);
  FFieldTypes.Add('INFO', 14);
  FFieldTypes.Add('TITLE', 15);
  FFieldTypes.Add('SUBJECT', 16);
  FFieldTypes.Add('AUTHOR', 17);
  FFieldTypes.Add('KEYWORDS', 18);
  FFieldTypes.Add('COMMENTS', 19);
  FFieldTypes.Add('LASTSAVEDBY', 20);
  FFieldTypes.Add('CREATEDATE', 21);
  FFieldTypes.Add('SAVEDATE', 22);
  FFieldTypes.Add('PRINTDATE', 23);
  FFieldTypes.Add('REVNUM', 24);
  FFieldTypes.Add('EDITTIME', 25);
  FFieldTypes.Add('NUMPAGES', 26);
  FFieldTypes.Add('NUMWORDS', 27);
  FFieldTypes.Add('NUMCHARS', 28);
  FFieldTypes.Add('FILENAME', 29);
  FFieldTypes.Add('TEMPLATE', 30);
  FFieldTypes.Add('DATE', 31);
  FFieldTypes.Add('TIME', 32);
  FFieldTypes.Add('PAGE', 33);
  FFieldTypes.Add('=', 34);
  FFieldTypes.Add('QUOTE', 35);
  FFieldTypes.Add('INCLUDE', 36);
  FFieldTypes.Add('PAGEREF', 37);
  FFieldTypes.Add('ASK', 38);
  FFieldTypes.Add('FILLIN', 39);
  FFieldTypes.Add('DATA', 40);
  FFieldTypes.Add('NEXT', 41);
  FFieldTypes.Add('NEXTIF', 42);
  FFieldTypes.Add('SKIPIF', 43);
  FFieldTypes.Add('MERGEREC', 44);
  FFieldTypes.Add('DDE', 45);
  FFieldTypes.Add('DDEAUTO', 46);
  FFieldTypes.Add('GLOSSARY', 47);
  FFieldTypes.Add('PRINT', 48);
  FFieldTypes.Add('EQ', 49);
  FFieldTypes.Add('GOTOBUTTON', 50);
  FFieldTypes.Add('MACROBUTTON', 51);
  FFieldTypes.Add('AUTONUMOUT', 52);
  FFieldTypes.Add('AUTONUMLGL', 53);
  FFieldTypes.Add('AUTONUM', 54);
  FFieldTypes.Add('IMPORT', 55);
  FFieldTypes.Add('LINK', 56);
  FFieldTypes.Add('SYMBOL', 57);
  FFieldTypes.Add('EMBED', 58);
  FFieldTypes.Add('MERGEFIELD', 59);
  FFieldTypes.Add('USERNAME', 60);
  FFieldTypes.Add('USERINITIALS', 61);
  FFieldTypes.Add('USERADDRESS', 62);
  FFieldTypes.Add('BARCODE', 63);
  FFieldTypes.Add('DOCVARIABLE', 64);
  FFieldTypes.Add('SECTION', 65);
  FFieldTypes.Add('SECTIONPAGES', 66);
  FFieldTypes.Add('INCLUDEPICTURE', 67);
  FFieldTypes.Add('INCLUDETEXT', 68);
  FFieldTypes.Add('FILESIZE', 69);
  FFieldTypes.Add('FORMTEXT', 70);
  FFieldTypes.Add('FORMCHECKBOX', 71);
  FFieldTypes.Add('NOTEREF', 72);
  FFieldTypes.Add('TOA', 73);
  FFieldTypes.Add('TA', 74);
  FFieldTypes.Add('MERGESEQ', 75);
  FFieldTypes.Add('MACRO', 76);

  FFieldTypes.Add('DATABASE', 78);
  FFieldTypes.Add('AUTOTEXT', 79);
  FFieldTypes.Add('COMPARE', 80);
  FFieldTypes.Add('ADDIN', 81);
  FFieldTypes.Add('FORMDROPDOWN', 83);
  FFieldTypes.Add('ADVANCE', 84);
  FFieldTypes.Add('DOCPROPERTY', 85);
  FFieldTypes.Add('CONTROL', 87);
  FFieldTypes.Add('HYPERLINK', 88);
  FFieldTypes.Add('AUTOTEXTLIST', 89);
  FFieldTypes.Add('LISTNUM', 90);
  FFieldTypes.Add('HTMLCONTROL', 91);
  FFieldTypes.Add('BIDIOUTLINE', 92);
  FFieldTypes.Add('ADDRESSBLOCK', 93);
  FFieldTypes.Add('GREETINGLINE', 94);
  FFieldTypes.Add('SHAPE', 95);
end;

class destructor TdxDocFieldBeginDescriptor.Finalize;
begin
  FEmpty.Free;
  FFieldTypes.Free;
end;

function TdxDocFieldBeginDescriptor.GetBoundaryType: Char;
begin
  Result := TdxTextCodes.FieldBegin;
end;

procedure TdxDocFieldBeginDescriptor.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(BoundaryType);
  AWriter.Write(FFieldType);
end;

function TdxDocFieldBeginDescriptor.CalcFieldType: TdxFieldType;
begin
  case FFieldType of
    RefFieldType:
      Result := TdxFieldType.Ref;
    NoteRefFieldType:
      Result := TdxFieldType.NoteRef;
    HyperlinkFieldType:
      Result := TdxFieldType.Hyperlink;
    else
      Result := TdxFieldType.None;
  end;
end;

function TdxDocFieldBeginDescriptor.CalcFieldType(const AVal: string): Byte;
begin
  if not FFieldTypes.TryGetValue(AVal, Result) then
    Result := UnknownFieldType;
end;

{ TdxDocFieldSeparatorDescriptor }

function TdxDocFieldSeparatorDescriptor.GetBoundaryType: Char;
begin
  Result := TdxTextCodes.FieldSeparator;
end;

procedure TdxDocFieldSeparatorDescriptor.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(BoundaryType);
  AWriter.Write(Byte($ff));
end;

{ TdxDocFieldDescriptorFactory }

class function TdxDocFieldDescriptorFactory.CreateInstance(AReader: TBinaryReader): IdxDocFieldDescriptor;
var
  AFieldCode, AInfo: Byte;
begin
  AFieldCode := AReader.ReadByte;
  AInfo := AReader.ReadByte;
  case AFieldCode and $7 of
    $3: Result := TdxDocFieldBeginDescriptor.Create(AInfo);
    $4: Result := TdxDocFieldSeparatorDescriptor.Create;
    $5: Result := TdxDocFieldEndDescriptor.Create(AInfo);
  else
    TdxDocImporter.ThrowInvalidDocFile;
    Result := nil;
  end;
end;

end.
