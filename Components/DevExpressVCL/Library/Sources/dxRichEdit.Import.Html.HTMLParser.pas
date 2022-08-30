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

unit dxRichEdit.Import.Html.HTMLParser;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.Options,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Core;

type

  TdxHtmlElementType = (
    Content,
    OpenTag,
    CloseTag,
    EmptyTag,
    Comment);

  TdxHtmlSpecialSymbolTable = TdxNamedOrdinalDictionary<char>;

  { TdxHtmlElement }

  TdxHtmlElement = class abstract(TdxReferencedObject)
  strict private
    FRawText: string;
  protected
    function GetElementType: TdxHtmlElementType; virtual; abstract;
  public
    property RawText: string read FRawText write FRawText;
    property ElementType: TdxHtmlElementType read GetElementType;
  end;

  TdxHtmlElementList = class(TdxReferencedObjectList<TdxHtmlElement>);

  TdxHtmlTagNameID = (
    Unknown,
    LI,
    TD,
    TR,
    TH,
    Table,
    NumberingList,
    BulletList,
    Style,
    Abbr,
    Acronym,
    Address,
    Area,
    BaseFont,
    Bdo,
    BgSound,
    Button,
    Cite,
    Dd,
    Del,
    Dfn,
    Dl,
    Dt,
    Embed,
    Fieldset,
    Form,
    Frame,
    FrameSet,
    Hr,
    Iframe,
    Input,
    Ins,
    Kbd,
    &Label,
    Legend,
    Map,
    Nobr,
    Noembed,
    NoFrames,
    NoScript,
    &Object,
    OptGroup,
    Option,
    Param,
    Q,
    Samp,
    Select,
    TextArea,
    TT,
    &Var,
    Wbr,
    Xmp,
    Html,
    Head,
    Base,
    Meta,
    Title,
    Link,
    Anchor,
    Body,
    Bold,
    Italic,
    Underline,
    Paragraph,
    Strong,
    Big,
    Small,
    Preformatted,
    Font,
    LineBreak,
    Emphasized,
    Img,
    Heading1,
    Heading2,
    Heading3,
    Heading4,
    Heading5,
    Heading6,
    SuperScript,
    SubScript,
    Center,
    S,
    Strike,
    Code,
    Span,
    &Div,
    Script,
    Blockquote,
    Caption,
    Thead,
    Tfoot,
    Tbody,
    Col,
    ColGroup);

  TdxHtmlTagNameIDTable = TdxNamedOrdinalDictionary<TdxHtmlTagNameID>;

  { TdxAttribute }

  TdxAttribute = class
  strict private
    FName: string;
    FValue: string;
  public
    function Clone: TdxAttribute;
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
  end;

  { TdxAttributeList }

  TdxAttributeList = class(TdxObjectList<TdxAttribute>)
  public
    procedure CopyFrom(ASource: TdxAttributeList);
  end;

  { TdxTag }

  TdxTag = class(TdxHtmlElement)
  strict private
    FNameID: TdxHtmlTagNameID;
    FType: TdxHtmlElementType;
    FAttributes: TdxAttributeList;
  protected
    function GetElementType: TdxHtmlElementType; override;
    procedure AppendCharToName(ASb: TStringBuilder; ACh: Char);
  public
    constructor Create(AType: TdxHtmlElementType);
    destructor Destroy; override;
    function CopyFrom(ATag: TdxTag): TdxTag;

    property NameID: TdxHtmlTagNameID read FNameID write FNameID;
    property Attributes: TdxAttributeList read FAttributes;
  end;

  { TdxComment }

  TdxComment = class(TdxHtmlElement)
  strict private
    FCommentText: string;
  protected
    function GetElementType: TdxHtmlElementType; override;
  public

    property CommentText: string read FCommentText write FCommentText;
  end;

  { TdxContent }

  TdxContent = class(TdxHtmlElement)
  strict private
    FContentText: string;
  protected
    function GetElementType: TdxHtmlElementType; override;
  public

    property ContentText: string read FContentText write FContentText;
  end;

  TdxState = (
    StartState,
    ReadContent,
    ReadTag,
    ReadOpenTag,
    ReadEmptyTag,
    ReadComment,
    ReadCommentContent,
    ReadTagName,
    ReadAttributeName,
    WaitAttributeName,
    WaitAttrNameOrEqualSymbol,
    WaitAttributeValue,
    ReadAttributeValue,
    ReadValueInQuotes,
    ReadValueInApostrophe,
    EndTag,
    StopReading);


  IdxStringBuilder = TdxChunkedStringBuilder;

  { TdxParserState }

  TdxParserState = class
  strict private
    FState: TdxState;
    FParentState: TdxState;
  protected
    procedure ChangeState(ANewState: TdxState);
  public
    constructor Create(AState: TdxState);

    property State: TdxState read FState;
    property ParentState: TdxState read FParentState;
  end;

  { TdxHtmlParser }

  TdxHtmlParser = class
  strict private
    class var
      FHtmlTagNameIDTable: TdxHtmlTagNameIDTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FRawText: IdxStringBuilder;
    FAttrText: TStringBuilder;
    FCommentText: TStringBuilder;
    FSpecialSymbol: string;
    FElementType: TdxHtmlElementType;
    FParserState: TdxParserState;
    FContent: TdxReferencedObjectContainer<TdxContent>;
    FElement: TdxReferencedObjectContainer<TdxHtmlElement>;
    FParserTag: TdxReferencedObjectContainer<TdxTag>;
    FParserTagName: TStringBuilder;
    FEncodingDetectionBuffer: TArray<Byte>;
    FEncodingDetectionBufferIndex: Integer;
    function GetAttr: TdxAttribute;
    function GetIsEncodingDetectionBufferFull: Boolean; inline;
    function CutCommentBash(const AComment: string): string;
    procedure SetContent(const Value: TdxContent); inline;
    procedure SetElement(const Value: TdxHtmlElement); inline;
    procedure SetParserTag(const Value: TdxTag); inline;
    function GetContent: TdxContent; inline;
    function GetElement: TdxHtmlElement; inline;
    function GetParserTag: TdxTag; inline;
  protected
    function GetHtmlTagNameIDTable: TdxHtmlTagNameIDTable; virtual;
    class function CreateHtmlTagNameTable: TdxHtmlTagNameIDTable; static;
    class function ConvertKeyToUpper(const AKey: string): string; static; inline;
    function IsEndState(AStream: TStreamReader): Boolean; inline;
    procedure StartState(ACh: Char);
    procedure ReadContent(ACh: Char);
    procedure ReadTag(ACh: Char);
    procedure ReadTagName(ACh: Char);
    procedure ReadComment(ACh: Char);
    procedure EndComment(const AComment: string);
    procedure ReadEmptyTag(ACh: Char);
    procedure WaitAttributeName(ACh: Char; ATag: TdxTag);
    procedure ForceEndElement;
    procedure ReadAttributeName(ACh: Char);
    procedure WaitAttrNameOrEqualSymbol(ACh: Char);
    procedure WaitAttributeValue(ACh: Char);
    procedure ReadAttributeValue(ACh: Char);
    procedure ReadValueInApostrophe(ACh: Char);
    procedure ReadValueInQuotes(ACh: Char);
    function IsTagClosed(ACh: Char): Boolean;
    function ProcessTagClose(ACh: Char): Boolean;
    procedure CreateTagElement; overload;
    procedure ToggleReadContentState(ACh: Char);
    procedure EndContentElement;
    function Read(AStream: TStreamReader): Char;
    procedure AppendToEncodingDetectionBuffer(AValue: Byte); inline;
    function CreateContentElement: TdxHtmlElement; virtual;
    function CreateCommentElement(const AComment: string): TdxHtmlElement; virtual;
    function CreateTagElement(ATag: TdxTag): TdxHtmlElement; overload; virtual;
    function ReplaceSpecialSymbolCore(const ARawText: string; AStartIndex: Integer; AIsPrevWhiteSpace: Boolean): string;
    function ReplaceSpecialSymbol(const ARawText: string): string;

    property RawText: IdxStringBuilder read FRawText;
    property AttrText: TStringBuilder read FAttrText;
    property CommentText: TStringBuilder read FCommentText;
    property SpecialSymbol: string read FSpecialSymbol write FSpecialSymbol;
    property ElementType: TdxHtmlElementType read FElementType write FElementType;
    property ParserState: TdxParserState read FParserState;
    property Content: TdxContent read GetContent write SetContent;
    property Element: TdxHtmlElement read GetElement write SetElement;
    property ParserTag: TdxTag read GetParserTag write SetParserTag;
    property ParserTagName: TStringBuilder read FParserTagName;
    property Attr: TdxAttribute read GetAttr;
    property IsEncodingDetectionBufferFull: Boolean read GetIsEncodingDetectionBufferFull;
    property HtmlTagNameIDTable: TdxHtmlTagNameIDTable read GetHtmlTagNameIDTable;
  public
    constructor Create;
    destructor Destroy; override;
    function GetTagNameID(const AName: string): TdxHtmlTagNameID;
    procedure ParseNextScript(AStream: TStreamReader; out AScriptContent, ACloseScriptTag: TdxHtmlElement);
    procedure ParseScriptContent(AStream: TStreamReader; out AScriptContent, ACloseScriptTag: TdxHtmlElement);
    function TryParseCloseScriptTag(AStream: TStreamReader; out AElement: TdxHtmlElement): Boolean;
    function ParseNext(AStream: TStreamReader): TdxHtmlElement;
    function ParseNextCore(AStream: TStreamReader): TdxHtmlElement;
    function IsReceivedElement(AStream: TStreamReader): Boolean;
    function DetectEncoding: TEncoding;
  end;

implementation

uses
  Contnrs, Rtti, TypInfo, Math, Character,
  dxStringHelper,
  dxEncoding;

{ TdxAttribute }

function TdxAttribute.Clone: TdxAttribute;
begin
  Result := TdxAttribute.Create;
  Result.FName := FName;
  Result.FValue := FValue;
end;

{ TdxAttributeList }

procedure TdxAttributeList.CopyFrom(ASource: TdxAttributeList);
var
  I: Integer;
begin
  Clear;
  for I := 0 to ASource.Count - 1 do
    Add(ASource[I].Clone);
end;

{ TdxTag }

constructor TdxTag.Create(AType: TdxHtmlElementType);
begin
  inherited Create;
  FAttributes := TdxAttributeList.Create;
  FType := AType;
  FNameID := TdxHtmlTagNameID.Unknown;
end;

destructor TdxTag.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited Destroy;
end;


function TdxTag.GetElementType: TdxHtmlElementType;
begin
  Result := FType;
end;

function TdxTag.CopyFrom(ATag: TdxTag): TdxTag;
begin
  NameID := ATag.NameID;
  FAttributes.CopyFrom(ATag.Attributes);
  Result := ATag;
end;

procedure TdxTag.AppendCharToName(ASb: TStringBuilder; ACh: Char);
begin
{$IFDEF DELPHIXE4}
  ASb.Append(ACh.ToUpper);
{$ELSE}
  ASb.Append(TCharacter.ToUpper(ACh));
{$ENDIF}
end;

{ TdxComment }

function TdxComment.GetElementType: TdxHtmlElementType;
begin
  Result := TdxHtmlElementType.Comment;
end;


{ TdxContent }

function TdxContent.GetElementType: TdxHtmlElementType;
begin
  Result := TdxHtmlElementType.Content;
end;


{ TdxParserState }

constructor TdxParserState.Create(AState: TdxState);
begin
  inherited Create;
  FState := AState;
end;

procedure TdxParserState.ChangeState(ANewState: TdxState);
begin
  FParentState := State;
  FState := ANewState;
end;

{ TdxHtmlParser }

constructor TdxHtmlParser.Create;
begin
  inherited Create;
  SetLength(FEncodingDetectionBuffer, 10000);
  FRawText := TdxChunkedStringBuilder.Create;
  FAttrText := TStringBuilder.Create;
  FCommentText := TStringBuilder.Create;
  FParserState := TdxParserState.Create(TdxState.StartState);

  FParserTagName := TStringBuilder.Create;
end;

destructor TdxHtmlParser.Destroy;
begin
  FRawText.Free;
  FAttrText.Free;
  FCommentText.Free;
  FParserState.Free;

  Content := nil;
  Element := nil;
  ParserTag := nil;
  FreeAndNil(FParserTagName);
  inherited Destroy;
end;

class constructor TdxHtmlParser.Initialize;
begin
  FHtmlTagNameIDTable := CreateHtmlTagNameTable;
end;

class destructor TdxHtmlParser.Finalize;
begin
  FHtmlTagNameIDTable.Free;
end;

class function TdxHtmlParser.ConvertKeyToUpper(const AKey: string): string;
begin
  Result := UpperCase(AKey);
end;

procedure TdxHtmlParser.SetContent(const Value: TdxContent);
begin
  FContent.Value := Value;
end;

procedure TdxHtmlParser.SetElement(const Value: TdxHtmlElement);
begin
  FElement.Value := Value;
end;

procedure TdxHtmlParser.SetParserTag(const Value: TdxTag);
begin
  FParserTag.Value := Value;
end;

function TdxHtmlParser.GetContent: TdxContent;
begin
  Result := FContent.Value;
end;

function TdxHtmlParser.GetElement: TdxHtmlElement;
begin
  Result := FElement.Value;
end;

function TdxHtmlParser.GetParserTag: TdxTag;
begin
  Result := FParserTag.Value;
end;

function TdxHtmlParser.GetAttr: TdxAttribute;
begin
  Result := ParserTag.Attributes[ParserTag.Attributes.Count - 1];
end;

function TdxHtmlParser.GetIsEncodingDetectionBufferFull: Boolean;
begin
  Result := FEncodingDetectionBufferIndex >= Length(FEncodingDetectionBuffer);
end;

function TdxHtmlParser.GetHtmlTagNameIDTable: TdxHtmlTagNameIDTable;
begin
  Result := FHtmlTagNameIDTable;
end;

class function TdxHtmlParser.CreateHtmlTagNameTable: TdxHtmlTagNameIDTable;
begin
  Result := TdxHtmlTagNameIDTable.Create;
  Result.Add(ConvertKeyToUpper('abbr'), TdxHtmlTagNameID.Abbr);
  Result.Add(ConvertKeyToUpper('acronym'), TdxHtmlTagNameID.Acronym);
  Result.Add(ConvertKeyToUpper('address'), TdxHtmlTagNameID.Address);
  Result.Add(ConvertKeyToUpper('area'), TdxHtmlTagNameID.Area);
  Result.Add(ConvertKeyToUpper('basefont'), TdxHtmlTagNameID.BaseFont);
  Result.Add(ConvertKeyToUpper('bdo'), TdxHtmlTagNameID.Bdo);
  Result.Add(ConvertKeyToUpper('bgsound'), TdxHtmlTagNameID.BgSound);
  Result.Add(ConvertKeyToUpper('Button'), TdxHtmlTagNameID.Button);
  Result.Add(ConvertKeyToUpper('cite'), TdxHtmlTagNameID.Cite);

  Result.Add(ConvertKeyToUpper('dd'), TdxHtmlTagNameID.Dd);
  Result.Add(ConvertKeyToUpper('del'), TdxHtmlTagNameID.Del);
  Result.Add(ConvertKeyToUpper('dfn'), TdxHtmlTagNameID.Dfn);
  Result.Add(ConvertKeyToUpper('dl'), TdxHtmlTagNameID.Dl);
  Result.Add(ConvertKeyToUpper('dt'), TdxHtmlTagNameID.Dt);
  Result.Add(ConvertKeyToUpper('embed'), TdxHtmlTagNameID.Embed);
  Result.Add(ConvertKeyToUpper('fieldset'), TdxHtmlTagNameID.Fieldset);
  Result.Add(ConvertKeyToUpper('form'), TdxHtmlTagNameID.Form);
  Result.Add(ConvertKeyToUpper('frame'), TdxHtmlTagNameID.Frame);
  Result.Add(ConvertKeyToUpper('frameset'), TdxHtmlTagNameID.FrameSet);
  Result.Add(ConvertKeyToUpper('hr'), TdxHtmlTagNameID.Hr);
  Result.Add(ConvertKeyToUpper('iframe'), TdxHtmlTagNameID.Iframe);
  Result.Add(ConvertKeyToUpper('input'), TdxHtmlTagNameID.Input);
  Result.Add(ConvertKeyToUpper('ins'), TdxHtmlTagNameID.Ins);
  Result.Add(ConvertKeyToUpper('kbd'), TdxHtmlTagNameID.Kbd);
  Result.Add(ConvertKeyToUpper('label'), TdxHtmlTagNameID.Label);
  Result.Add(ConvertKeyToUpper('legend'), TdxHtmlTagNameID.Legend);
  Result.Add(ConvertKeyToUpper('map'), TdxHtmlTagNameID.Map);
  Result.Add(ConvertKeyToUpper('nobr'), TdxHtmlTagNameID.Nobr);
  Result.Add(ConvertKeyToUpper('noembed'), TdxHtmlTagNameID.Noembed);
  Result.Add(ConvertKeyToUpper('noframes'), TdxHtmlTagNameID.NoFrames);
  Result.Add(ConvertKeyToUpper('noscript'), TdxHtmlTagNameID.NoScript);
  Result.Add(ConvertKeyToUpper('object'), TdxHtmlTagNameID.Object);
  Result.Add(ConvertKeyToUpper('optgroup'), TdxHtmlTagNameID.OptGroup);
  Result.Add(ConvertKeyToUpper('option'), TdxHtmlTagNameID.Option);
  Result.Add(ConvertKeyToUpper('param'), TdxHtmlTagNameID.Param);
  Result.Add(ConvertKeyToUpper('q'), TdxHtmlTagNameID.Q);
  Result.Add(ConvertKeyToUpper('samp'), TdxHtmlTagNameID.Samp);
  Result.Add(ConvertKeyToUpper('select'), TdxHtmlTagNameID.Select);
  Result.Add(ConvertKeyToUpper('textarea'), TdxHtmlTagNameID.TextArea);
  Result.Add(ConvertKeyToUpper('tt'), TdxHtmlTagNameID.TT);
  Result.Add(ConvertKeyToUpper('var'), TdxHtmlTagNameID.Var);
  Result.Add(ConvertKeyToUpper('wbr'), TdxHtmlTagNameID.Wbr);
  Result.Add(ConvertKeyToUpper('xmp'), TdxHtmlTagNameID.Xmp);

  Result.Add(ConvertKeyToUpper('html'), TdxHtmlTagNameID.Html);
  Result.Add(ConvertKeyToUpper('head'), TdxHtmlTagNameID.Head);
  Result.Add(ConvertKeyToUpper('base'), TdxHtmlTagNameID.Base);
  Result.Add(ConvertKeyToUpper('meta'), TdxHtmlTagNameID.Meta);
  Result.Add(ConvertKeyToUpper('title'), TdxHtmlTagNameID.Title);
  Result.Add(ConvertKeyToUpper('link'), TdxHtmlTagNameID.Link);
  Result.Add(ConvertKeyToUpper('a'), TdxHtmlTagNameID.Anchor);
  Result.Add(ConvertKeyToUpper('body'), TdxHtmlTagNameID.Body);
  Result.Add(ConvertKeyToUpper('b'), TdxHtmlTagNameID.Bold);
  Result.Add(ConvertKeyToUpper('i'), TdxHtmlTagNameID.Italic);
  Result.Add(ConvertKeyToUpper('u'), TdxHtmlTagNameID.Underline);
  Result.Add(ConvertKeyToUpper('p'), TdxHtmlTagNameID.Paragraph);
  Result.Add(ConvertKeyToUpper('strong'), TdxHtmlTagNameID.Strong);
  Result.Add(ConvertKeyToUpper('big'), TdxHtmlTagNameID.Big);
  Result.Add(ConvertKeyToUpper('small'), TdxHtmlTagNameID.Small);
  Result.Add(ConvertKeyToUpper('pre'), TdxHtmlTagNameID.Preformatted);
  Result.Add(ConvertKeyToUpper('font'), TdxHtmlTagNameID.Font);
  Result.Add(ConvertKeyToUpper('br'), TdxHtmlTagNameID.LineBreak);
  Result.Add(ConvertKeyToUpper('em'), TdxHtmlTagNameID.Emphasized);
  Result.Add(ConvertKeyToUpper('img'), TdxHtmlTagNameID.Img);
  Result.Add(ConvertKeyToUpper('h1'), TdxHtmlTagNameID.Heading1);
  Result.Add(ConvertKeyToUpper('h2'), TdxHtmlTagNameID.Heading2);
  Result.Add(ConvertKeyToUpper('h3'), TdxHtmlTagNameID.Heading3);
  Result.Add(ConvertKeyToUpper('h4'), TdxHtmlTagNameID.Heading4);
  Result.Add(ConvertKeyToUpper('h5'), TdxHtmlTagNameID.Heading5);
  Result.Add(ConvertKeyToUpper('h6'), TdxHtmlTagNameID.Heading6);
  Result.Add(ConvertKeyToUpper('sup'), TdxHtmlTagNameID.SuperScript);
  Result.Add(ConvertKeyToUpper('sub'), TdxHtmlTagNameID.SubScript);
  Result.Add(ConvertKeyToUpper('center'), TdxHtmlTagNameID.Center);
  Result.Add(ConvertKeyToUpper('table'), TdxHtmlTagNameID.Table);

  Result.Add(ConvertKeyToUpper('tr'), TdxHtmlTagNameID.TR);
  Result.Add(ConvertKeyToUpper('th'), TdxHtmlTagNameID.TH);
  Result.Add(ConvertKeyToUpper('td'), TdxHtmlTagNameID.TD);
  Result.Add(ConvertKeyToUpper('li'), TdxHtmlTagNameID.LI);
  Result.Add(ConvertKeyToUpper('ol'), TdxHtmlTagNameID.NumberingList);
  Result.Add(ConvertKeyToUpper('ul'), TdxHtmlTagNameID.BulletList);
  Result.Add(ConvertKeyToUpper('s'), TdxHtmlTagNameID.S);
  Result.Add(ConvertKeyToUpper('strike'), TdxHtmlTagNameID.Strike);
  Result.Add(ConvertKeyToUpper('code'), TdxHtmlTagNameID.Code);
  Result.Add(ConvertKeyToUpper('span'), TdxHtmlTagNameID.Span);
  Result.Add(ConvertKeyToUpper('div'), TdxHtmlTagNameID.Div);
  Result.Add(ConvertKeyToUpper('script'), TdxHtmlTagNameID.Script);
  Result.Add(ConvertKeyToUpper('blockquote'), TdxHtmlTagNameID.Blockquote);
  Result.Add(ConvertKeyToUpper('caption'), TdxHtmlTagNameID.Caption);
  Result.Add(ConvertKeyToUpper('THEAD'), TdxHtmlTagNameID.Thead);
  Result.Add(ConvertKeyToUpper('TFOOT'), TdxHtmlTagNameID.Tfoot);
  Result.Add(ConvertKeyToUpper('TBODY'), TdxHtmlTagNameID.Tbody);
  Result.Add(ConvertKeyToUpper('COL'), TdxHtmlTagNameID.Col);
  Result.Add(ConvertKeyToUpper('COLGROUP'), TdxHtmlTagNameID.ColGroup);
  Result.Add(ConvertKeyToUpper('Style'), TdxHtmlTagNameID.Style);
end;

function TdxHtmlParser.ParseNext(AStream: TStreamReader): TdxHtmlElement;
begin
  Result := ParseNextCore(AStream);
end;

procedure TdxHtmlParser.ParseNextScript(AStream: TStreamReader; out AScriptContent, ACloseScriptTag: TdxHtmlElement);
begin
  ParseScriptContent(AStream, AScriptContent, ACloseScriptTag);
end;

procedure TdxHtmlParser.ParseScriptContent(AStream: TStreamReader; out AScriptContent, ACloseScriptTag: TdxHtmlElement);
var
  ARawText: TdxChunkedStringBuilder;
  AIntChar: Integer;
  ACh: Char;
  ATagEnd: Boolean;
  AText: string;
begin
  AScriptContent := nil;
  ACloseScriptTag := nil;
  ARawText := TdxChunkedStringBuilder.Create;
  try
    while True do
    begin
      AIntChar := AStream.Read;
      if AIntChar < 0 then
      begin
        if ARawText.Length = 0 then
          Exit;
        Break;
      end;
      ACh := Char(AIntChar);
      ATagEnd := (ACh = '<') and (Char(AStream.Peek) = '/');
      if ATagEnd then
      begin
        RawText.Clear;
        if TryParseCloseScriptTag(AStream, ACloseScriptTag) then
          Break
        else
        begin
          ARawText.Append(RawText.ToString);
          RawText.Clear;
        end;
      end
      else
        ARawText.Append(ACh);
    end;

    Content := TdxContent.Create;
    AText := ARawText.ToString;
  finally
    ARawText.Free;
  end;
  Content.ContentText := AText;
  Content.RawText := AText;
  AScriptContent := Content;
end;

function TdxHtmlParser.TryParseCloseScriptTag(AStream: TStreamReader; out AElement: TdxHtmlElement): Boolean;
var
  ATag: TdxTag;
begin
  AElement := nil;
  if AStream.EndOfStream then
    Exit(False);

  ParserState.ChangeState(TdxState.ReadTag);
  RawText.Append('<');
  if IsReceivedElement(AStream) then
  begin
    ATag := Safe<TdxTag>.Cast(Element);
    if (ATag <> nil) and (ATag.NameID = TdxHtmlTagNameID.Script) then
    begin
      AElement := Element;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TdxHtmlParser.ParseNextCore(AStream: TStreamReader): TdxHtmlElement;
var
  AIntChar: Integer;
begin
  ParserState.ChangeState(TdxState.StartState);
  FRawText.Clear;
  AIntChar := AStream.Peek;
  if AIntChar < 0 then
    Exit(nil);

  if IsReceivedElement(AStream) then
    Result := Element
  else
    Result := CreateContentElement;
end;

function TdxHtmlParser.IsEndState(AStream: TStreamReader): Boolean;
begin
  case ParserState.State of
    TdxState.EndTag:
      begin
        Read(AStream);
        Element.RawText := RawText.ToString;
        Result := True;
      end;
    TdxState.StopReading:
      begin
        Element.RawText := RawText.ToString;
        Result := True;
      end;
    else
      Result := False;
  end;
end;

function TdxHtmlParser.IsReceivedElement(AStream: TStreamReader): Boolean;
var
  AIntChar: Integer;
  ACh: Char;
begin
  AIntChar := AStream.Peek;
  while AIntChar >= 0 do
  begin
    ACh := Char(AIntChar);
    case ParserState.State of
      TdxState.StartState:
        StartState(ACh);
      TdxState.ReadContent:
        ReadContent(ACh);
      TdxState.ReadTag:
        ReadTag(ACh);
      TdxState.ReadEmptyTag:
        ReadEmptyTag(ACh);
      TdxState.ReadComment:
        ReadComment(ACh);
      TdxState.ReadTagName:
        ReadTagName(ACh);
      TdxState.ReadAttributeName:
        ReadAttributeName(ACh);
      TdxState.WaitAttributeName:
        WaitAttributeName(ACh, ParserTag);
      TdxState.WaitAttrNameOrEqualSymbol:
        WaitAttrNameOrEqualSymbol(ACh);
      TdxState.WaitAttributeValue:
        WaitAttributeValue(ACh);
      TdxState.ReadAttributeValue:
        ReadAttributeValue(ACh);
      TdxState.ReadValueInQuotes:
        ReadValueInQuotes(ACh);
      TdxState.ReadValueInApostrophe:
        ReadValueInApostrophe(ACh);
    end;
    if IsEndState(AStream) then
      Exit(True);
    Read(AStream);
    AIntChar := AStream.Peek;
  end;
  Result := False;
end;

procedure TdxHtmlParser.StartState(ACh: Char);
begin
  if ACh = '<' then
    ParserState.ChangeState(TdxState.ReadTag)
  else
    ParserState.ChangeState(TdxState.ReadContent);
end;

procedure TdxHtmlParser.ReadContent(ACh: Char);
begin
  ToggleReadContentState(ACh);
end;

procedure TdxHtmlParser.ReadTag(ACh: Char);
begin
  case ACh of
    '!':
      ParserState.ChangeState(TdxState.ReadComment);
    '/':
      begin
        ParserTag := TdxTag.Create(TdxHtmlElementType.CloseTag);
        ParserTagName.Clear;
        ParserState.ChangeState(TdxState.ReadTagName);
      end;
    else
      if not {$IFDEF DELPHIXE4}ACh.IsLetter{$ELSE}TCharacter.IsLetter(ACh){$ENDIF} and (ACh <> '?') then
      begin
        ParserState.ChangeState(TdxState.ReadContent);
        ToggleReadContentState(ACh);
      end
      else
      begin
        ParserTag := TdxTag.Create(TdxHtmlElementType.OpenTag);
        ParserTagName.Clear;
        ParserState.ChangeState(TdxState.ReadTagName);
        ParserTag.AppendCharToName(FParserTagName, ACh);
      end;
  end;
end;

procedure TdxHtmlParser.ReadTagName(ACh: Char);
begin
  if IsTagClosed(ACh) then
    Exit;
  if ACh = '<' then
    EndContentElement
  else
    if {$IFDEF DELPHIXE4}ACh.IsWhiteSpace{$ELSE}TCharacter.IsWhiteSpace(ACh){$ENDIF} then
    begin
      ParserTag.NameID := GetTagNameID(ParserTagName.ToString);

      ParserState.ChangeState(TdxState.WaitAttributeName);
    end
    else
      ParserTag.AppendCharToName(FParserTagName, ACh);
end;

procedure TdxHtmlParser.ReadComment(ACh: Char);
var
  AComment: string;
begin
  if ACh = '>' then
  begin
    AComment := CommentText.ToString;
    if TdxStringHelper.StartsWith(AComment, '--', False) then
    begin
      if not TdxStringHelper.EndsWith(AComment, '--', False) then
        Exit;
      AComment := CutCommentBash(AComment);
      EndComment(AComment);
    end
    else
      EndComment(AComment);
  end
  else
    CommentText.Append(ACh);
end;

function TdxHtmlParser.CutCommentBash(const AComment: string): string;
begin
  if Length(AComment) < 4 then
    Exit(AComment);
  Result := AComment;
  Delete(Result, Length(AComment) - 1, 2);
  Delete(Result, 1, 2);
end;

procedure TdxHtmlParser.EndComment(const AComment: string);
begin
  Element := CreateCommentElement(AComment);
  CommentText.Length := 0;
  ParserState.ChangeState(TdxState.EndTag);
end;

procedure TdxHtmlParser.ReadEmptyTag(ACh: Char);
var
  AEmptyTag: TdxTag;
begin
  if ACh = '>' then
  begin
    AEmptyTag := TdxTag.Create(TdxHtmlElementType.EmptyTag);
    ParserTag.NameID := GetTagNameID(ParserTagName.ToString);
    AEmptyTag.CopyFrom(ParserTag);
    ParserTagName.Clear;
    Element := CreateTagElement(AEmptyTag);
    ParserState.ChangeState(TdxState.EndTag);
  end;
end;

procedure TdxHtmlParser.WaitAttributeName(ACh: Char; ATag: TdxTag);
var
  AAttr: TdxAttribute;
  AAttributes: TdxList<TdxAttribute>;
begin
  if IsTagClosed(ACh) then
    Exit;
  if {$IFDEF DELPHIXE4}ACh.IsWhiteSpace{$ELSE}TCharacter.IsWhiteSpace(ACh){$ENDIF} then
    Exit;
  if ACh = '<' then
  begin
    ForceEndElement;
    Exit;
  end;
  AAttr := TdxAttribute.Create;
  AttrText.Length := 0;
  AAttributes := ATag.Attributes;
  AAttributes.Add(AAttr);
  ParserState.ChangeState(TdxState.ReadAttributeName);
  if ACh = '=' then
    ParserState.ChangeState(TdxState.WaitAttributeValue)
  else
    AttrText.Append(ACh);
end;

procedure TdxHtmlParser.ForceEndElement;
begin
  CreateTagElement;
  ParserState.ChangeState(TdxState.StopReading);
end;

procedure TdxHtmlParser.ReadAttributeName(ACh: Char);
begin
  if IsTagClosed(ACh) then
  begin
    Attr.Name := UpperCase(AttrText.ToString);
  end
  else
    if ACh = '<' then
    begin
      ForceEndElement;
    end
    else
      if {$IFDEF DELPHIXE4}ACh.IsWhiteSpace{$ELSE}TCharacter.IsWhiteSpace(ACh){$ENDIF} then
      begin
        Attr.Name := UpperCase(AttrText.ToString);
        ParserState.ChangeState(TdxState.WaitAttrNameOrEqualSymbol);
      end
      else
        if ACh = '=' then
        begin
          Attr.Name := UpperCase(AttrText.ToString);
          ParserState.ChangeState(TdxState.WaitAttributeValue);
        end
        else
          AttrText.Append(ACh);
end;

procedure TdxHtmlParser.WaitAttrNameOrEqualSymbol(ACh: Char);
begin
  if IsTagClosed(ACh) then
    Exit;
  if {$IFDEF DELPHIXE4}ACh.IsWhiteSpace{$ELSE}TCharacter.IsWhiteSpace(ACh){$ENDIF} then
    Exit;
  if ACh = '=' then
    ParserState.ChangeState(TdxState.WaitAttributeValue)
  else
    if ACh = '<' then
      ForceEndElement
    else
      WaitAttributeName(ACh, ParserTag);
end;

procedure TdxHtmlParser.WaitAttributeValue(ACh: Char);
begin
  AttrText.Length := 0;
  if IsTagClosed(ACh) then
    Exit;
  if {$IFDEF DELPHIXE4}ACh.IsWhiteSpace{$ELSE}TCharacter.IsWhiteSpace(ACh){$ENDIF} then
    Exit;

  case ACh of
    '<':
      ForceEndElement;
    #$27:
      ParserState.ChangeState(TdxState.ReadValueInApostrophe);
    '"':
      ParserState.ChangeState(TdxState.ReadValueInQuotes);
    else
    begin
      ParserState.ChangeState(TdxState.ReadAttributeValue);
      AttrText.Append(ACh);
    end;
  end;
end;

procedure TdxHtmlParser.ReadAttributeValue(ACh: Char);
begin
  if ProcessTagClose(ACh) then
  begin
    Attr.Value := Trim(AttrText.ToString);
  end
  else
    if ACh = '<' then
      ForceEndElement
    else
      if {$IFDEF DELPHIXE4}ACh.IsWhiteSpace{$ELSE}TCharacter.IsWhiteSpace(ACh){$ENDIF} then
      begin
        Attr.Value := Trim(AttrText.ToString);
        ParserState.ChangeState(TdxState.WaitAttributeName);
      end
      else
        AttrText.Append(ACh);
end;

procedure TdxHtmlParser.ReadValueInApostrophe(ACh: Char);
begin
  if ACh = #$27 then
  begin
    ParserState.ChangeState(TdxState.WaitAttributeName);
    Attr.Value := Trim(AttrText.ToString);
  end
  else
    AttrText.Append(ACh);
end;

procedure TdxHtmlParser.ReadValueInQuotes(ACh: Char);
begin
  if ACh = '"' then
  begin
    ParserState.ChangeState(TdxState.WaitAttributeName);
    Attr.Value := Trim(AttrText.ToString);
  end
  else
    AttrText.Append(ACh);
end;

function TdxHtmlParser.IsTagClosed(ACh: Char): Boolean;
begin
  if ProcessTagClose(ACh) then
    Exit(True);

  if (ACh = '/') and (ParserTag.ElementType = TdxHtmlElementType.OpenTag) then
  begin
    ParserState.ChangeState(TdxState.ReadEmptyTag);
    Exit(True);
  end;
  Result := False;
end;

function TdxHtmlParser.ProcessTagClose(ACh: Char): Boolean;
begin
  if ACh = '>' then
  begin
    CreateTagElement;
    ParserState.ChangeState(TdxState.EndTag);
    Exit(True);
  end;
  Result := False;
end;

procedure TdxHtmlParser.CreateTagElement;
begin
  Element := CreateTagElement(ParserTag);
  if ParserTagName.Length <> 0 then
  begin
    ParserTag.NameID := GetTagNameID(ParserTagName.ToString);
    ParserTagName.Length := 0;
  end;
end;

procedure TdxHtmlParser.ToggleReadContentState(ACh: Char);
begin
  if ACh = '<' then
    EndContentElement
  else
    ParserState.ChangeState(TdxState.ReadContent);
end;

procedure TdxHtmlParser.EndContentElement;
begin
  Element := CreateContentElement;
  ParserState.ChangeState(TdxState.StopReading);
end;

procedure TdxHtmlParser.AppendToEncodingDetectionBuffer(AValue: Byte);
begin
  FEncodingDetectionBuffer[FEncodingDetectionBufferIndex] := AValue;
  Inc(FEncodingDetectionBufferIndex);
end;

function TdxHtmlParser.Read(AStream: TStreamReader): Char;
var
  ACh: Char;
begin
  ACh := Char(AStream.Read);
  if not IsEncodingDetectionBufferFull then
    AppendToEncodingDetectionBuffer(Byte(ACh));
  RawText.Append(ACh);
  Result := ACh;
end;

function TdxHtmlParser.DetectEncoding: TEncoding;
begin
  Result := TdxEncoding.DetectEncoding(FEncodingDetectionBuffer, 0,
    Min(Length(FEncodingDetectionBuffer), FEncodingDetectionBufferIndex));
end;

function TdxHtmlParser.CreateContentElement: TdxHtmlElement;
var
  AText: string;
begin
  Content := TdxContent.Create;
  AText := ReplaceSpecialSymbol(RawText.ToString);
  Content.ContentText := AText;
  Content.RawText := AText;
  Result := Content;
end;

function TdxHtmlParser.CreateCommentElement(const AComment: string): TdxHtmlElement;
var
  AElement: TdxComment;
begin
  AElement := TdxComment.Create;
  AElement.CommentText := AComment;
  AElement.RawText := ReplaceSpecialSymbol(RawText.ToString);
  Result := AElement;
end;

function TdxHtmlParser.CreateTagElement(ATag: TdxTag): TdxHtmlElement;
begin
  ATag.RawText := ReplaceSpecialSymbol(RawText.ToString);
  Result := ATag;
end;

function TdxHtmlParser.ReplaceSpecialSymbolCore(const ARawText: string; AStartIndex: Integer; AIsPrevWhiteSpace: Boolean): string;
var
  I: Integer;
  ACh: Char;
  AText: TStringBuilder;
begin
  AText := TStringBuilder.Create;
  try
    AText.Append(ARawText, 0, AStartIndex - 1);
    for I := AStartIndex to Length(ARawText) do
    begin
      ACh := ARawText[I];
      if (ACh = #10) or (ACh = #13) or (ACh = #9) or (ACh = ' ') or (ACh = #0) then
      begin
        if not AIsPrevWhiteSpace then
        begin
          AText.Append(' ');
          AIsPrevWhiteSpace := True;
        end;
      end
      else
      begin
        AIsPrevWhiteSpace := False;
        AText.Append(ACh);
      end;
    end;
    Result := AText.ToString;
  finally
    AText.Free;
  end;
end;

function TdxHtmlParser.ReplaceSpecialSymbol(const ARawText: string): string;
var
  AIsPrevWhiteSpace, AIsSpecialWhiteSpace: Boolean;
  I: Integer;
  ACh: Char;
begin
  AIsPrevWhiteSpace := False;
  for I := 1 to Length(ARawText) do
  begin
    ACh := ARawText[I];
    AIsSpecialWhiteSpace := (ACh = #10) or (ACh = #13) or (ACh = #9) or (ACh = #0);
    if AIsSpecialWhiteSpace or (ACh = ' ') then
    begin
      if AIsPrevWhiteSpace or AIsSpecialWhiteSpace then
        Exit(ReplaceSpecialSymbolCore(ARawText, I, AIsPrevWhiteSpace));
      AIsPrevWhiteSpace := True;
    end
    else
      AIsPrevWhiteSpace := False;
  end;
  Result := ARawText;
end;

function TdxHtmlParser.GetTagNameID(const AName: string): TdxHtmlTagNameID;
var
  AResult: TdxHtmlTagNameID;
begin
  if HtmlTagNameIDTable.TryGetValue(AName, AResult) then
    Exit(AResult);
  Result := TdxHtmlTagNameID.Unknown;
end;

end.
