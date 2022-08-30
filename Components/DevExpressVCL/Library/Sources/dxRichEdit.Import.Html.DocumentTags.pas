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

unit dxRichEdit.Import.Html.DocumentTags;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Generics.Defaults, Generics.Collections,
  dxCoreGraphics,
  dxGenerics,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.Import,
  dxRichEdit.Import.Html.TagBase;

type
  { TdxHtmlTag }

  TdxHtmlTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  public
    procedure ApplyProperties; override;
  end;

  { TdxHeadTag }

  TdxHeadTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxIgnoredTag }

  TdxIgnoredTag = class abstract(TdxTagBase)
  protected
    function GetShouldBeIgnored: Boolean; override;
    procedure ApplyTagProperties; override;
  public
    procedure EmptyTagProcess; override;
  end;

  { TdxTitleTag }

  TdxTitleTag = class(TdxIgnoredTag);

  { TdxLinkTag }

  TdxLinkTag = class(TdxTagBase)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FType: string;
    FHref: string;
    FMediaDescriptors: TdxStringList;
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
    class procedure HrefKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure TypeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure MediaKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    procedure SetMediaDescriptors(const Value: TdxStringList);
  protected
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    procedure ApplyTagProperties; override;
    procedure OpenTagProcessCore; override;

    property &Type: string read FType write FType;
    property Href: string read FHref write FHref;
    property MediaDescriptors: TdxStringList read FMediaDescriptors write SetMediaDescriptors;
  public
    constructor Create(AImporter: TdxCustomHtmlImporter);
    destructor Destroy; override;
  end;

  { TdxMetaTag }

  TdxMetaTag = class(TdxTagBase)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FCharset: string;
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
    class procedure CharsetKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure ContentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure HttpEquivKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure NameKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
  protected
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    procedure ApplyTagProperties; override;
    procedure OpenTagProcessCore; override;

    property Charset: string read FCharset write FCharset;
  end;

  { TdxBodyTag }

  TdxBodyTag = class(TdxTagBase)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FBgColor: TdxAlphaColor;
    FTextColor: TdxAlphaColor;
    FUseTextColor: Boolean;
    FUseBgColor: Boolean;
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
    class procedure ActiveLinkKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure BackGroundKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure BackGroundColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure OpenLinkColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure TextColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure LinkColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
  protected
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    procedure ApplyTagProperties; override;
    function ApplyCssProperties: TdxParagraphFormattingOptions; override;
    procedure ApplyDocumentModelWebSettings;
    procedure OpenTagProcessCore; override;
    procedure ApplyPageBackColorProperties; virtual;
  public
    constructor Create(AImporter: TdxCustomHtmlImporter);
  end;

implementation

uses
  SysUtils, Classes, RegularExpressions,
  dxRichEdit.NativeApi,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.Import.CSSSelectorParser,
  dxRichEdit.Import.Html,
  dxRichEdit.Utils.UriStreamService,
  dxUriRecord,
  dxStringHelper,
  dxEncoding,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.NumberParser;

{ TdxHtmlTag }

procedure TdxHtmlTag.ApplyTagProperties;
begin
end;

procedure TdxHtmlTag.ApplyProperties;
begin
  inherited ApplyProperties;
  TdxHtmlImporter(Importer).RootDoubleFontSize := TdxHtmlImporter(Importer).Position.CharacterFormatting.DoubleFontSize;
end;

{ TdxHeadTag }

procedure TdxHeadTag.ApplyTagProperties;
begin
end;

{ TdxIgnoredTag }

function TdxIgnoredTag.GetShouldBeIgnored: Boolean;
begin
  Result := True;
end;

procedure TdxIgnoredTag.ApplyTagProperties;
begin
end;

procedure TdxIgnoredTag.EmptyTagProcess;
begin
  TdxHtmlImporter(Importer).CloseProcess(Self);
end;

{ TdxLinkTag }

constructor TdxLinkTag.Create(AImporter: TdxCustomHtmlImporter);
begin
  inherited Create(AImporter);
  FMediaDescriptors := TdxStringList.Create;
end;

destructor TdxLinkTag.Destroy;
begin
  FreeAndNil(FMediaDescriptors);
  inherited Destroy;
end;

class constructor TdxLinkTag.Initialize;
begin
  FAttributeTable := AddAttributes;
end;

class destructor TdxLinkTag.Finalize;
begin
  FAttributeTable.Free;
end;

class function TdxLinkTag.AddAttributes: TdxAttributeKeywordTranslatorTable;
begin
  Result := CreateAttributeTable;

  Result.Add(ConvertKeyToUpper('href'), HrefKeyword);
  Result.Add(ConvertKeyToUpper('type'), TypeKeyword);
  Result.Add(ConvertKeyToUpper('media'), MediaKeyword);
end;

class procedure TdxLinkTag.HrefKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ALinkTag: TdxLinkTag;
begin
  ALinkTag := TdxLinkTag(ATag);
  ALinkTag.Href := AValue;
end;

class procedure TdxLinkTag.TypeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ALinkTag: TdxLinkTag;
begin
  ALinkTag := TdxLinkTag(ATag);
  ALinkTag.&Type := AValue;
end;

class procedure TdxLinkTag.MediaKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ALinkTag: TdxLinkTag;
begin
  ALinkTag := TdxLinkTag(ATag);
  ALinkTag.MediaDescriptors := ParseMediaAttribute(UpperCase(AValue));
end;

function TdxLinkTag.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

procedure TdxLinkTag.ApplyTagProperties;
begin
end;

procedure TdxLinkTag.OpenTagProcessCore;
var
  AStreamService: IdxUriStreamService;
  AUri: string;
  AStream: TStream;
  AReader: TStreamReader;
begin
  if SameText(FType, 'text/css') and ((FMediaDescriptors.Contains('SCREEN')) or
    (FMediaDescriptors.Contains('ALL')) or (FMediaDescriptors.Count = 0)) then
  begin
    AStreamService := Importer.DocumentModel.GetService<IdxUriStreamService>;
    if AStreamService = nil then
      Exit;
    AUri := GetAbsoluteUri(TdxHtmlImporter(Importer).AbsoluteBaseUri, FHref);
    AStream := AStreamService.GetStream(AUri);
    if AStream <> nil then
    try
      AReader := TStreamReader.Create(AStream);
      try
        TdxHtmlImporter(Importer).ParseCssElementCollection(AReader);
      finally
        AReader.Free;
      end;
    finally
      AStream.Free;
    end;
  end;
end;

procedure TdxLinkTag.SetMediaDescriptors(const Value: TdxStringList);
begin
  if FMediaDescriptors = Value then
    Exit;
  FMediaDescriptors.Free;
  FMediaDescriptors := Value;
end;

{ TdxMetaTag }

class constructor TdxMetaTag.Initialize;
begin
  FAttributeTable := AddAttributes;
end;

class destructor TdxMetaTag.Finalize;
begin
  FAttributeTable.Free;
end;

class function TdxMetaTag.AddAttributes: TdxAttributeKeywordTranslatorTable;
begin
  Result := CreateAttributeTable;
  Result.Add(ConvertKeyToUpper('charset'), CharsetKeyword);
  Result.Add(ConvertKeyToUpper('content'), ContentKeyword);
  Result.Add(ConvertKeyToUpper('http-equiv'), HttpEquivKeyword);
  Result.Add(ConvertKeyToUpper('name'), NameKeyword);
end;

class procedure TdxMetaTag.CharsetKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  AMetaTag: TdxMetaTag;
begin
  AMetaTag := TdxMetaTag(ATag);
  AMetaTag.Charset := AValue;
end;

class procedure TdxMetaTag.ContentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  AMetaTag: TdxMetaTag;
  AValues: TArray<string>;
  I: Integer;
  AMatch: TMatch;
begin
  AMetaTag := TdxMetaTag(ATag);
  AValues := TdxStringHelper.Split(AValue, [';']);
  for I := 0 to Length(AValues) - 1 do
  begin
    AMatch := TdxAttributePattern.Regex.Match(AValues[I]);
    if (UpperCase(TdxAttributePattern.TryGetGroupValue(AMatch, 'attrName')) = 'CHARSET') and
      (TdxAttributePattern.TryGetGroupValue(AMatch, 'attrEq') = '=') then
      AMetaTag.Charset := TdxAttributePattern.TryGetGroupValue(AMatch, 'attrValue');
  end;
end;

class procedure TdxMetaTag.HttpEquivKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxMetaTag.NameKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

function TdxMetaTag.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

procedure TdxMetaTag.ApplyTagProperties;
begin
end;

procedure TdxMetaTag.OpenTagProcessCore;
var
  AEncoding: TEncoding;
begin
  if FCharset = '' then
    Exit;

  if TdxHtmlImporter(Importer).Options.IgnoreMetaCharset then
    Exit;

  AEncoding := TdxEncoding.GetEncoding(FCharset);
  if AEncoding <> nil then
    TdxHtmlImporter(Importer).CodePage := AEncoding.CodePage;
end;

{ TdxBodyTag }

constructor TdxBodyTag.Create(AImporter: TdxCustomHtmlImporter);
begin
  inherited Create(AImporter);
  if TdxHtmlImporter(Importer).Position.CharacterFormatting.Options.UseForeColor then
  begin
    FTextColor := TdxHtmlImporter(Importer).Position.CharacterFormatting.ForeColor;
    FUseTextColor := True;
  end;
end;

class constructor TdxBodyTag.Initialize;
begin
  FAttributeTable := AddAttributes;
end;

class destructor TdxBodyTag.Finalize;
begin
  FAttributeTable.Free;
end;

class function TdxBodyTag.AddAttributes: TdxAttributeKeywordTranslatorTable;
begin
  Result := CreateAttributeTable;
  Result.Add(ConvertKeyToUpper('alink'), ActiveLinkKeyword);
  Result.Add(ConvertKeyToUpper('background'), BackGroundKeyword);
  Result.Add(ConvertKeyToUpper('bgcolor'), BackGroundColorKeyword);
  Result.Add(ConvertKeyToUpper('link'), LinkColorKeyword);
  Result.Add(ConvertKeyToUpper('text'), TextColorKeyword);
  Result.Add(ConvertKeyToUpper('vlink'), OpenLinkColorKeyword);
end;

class procedure TdxBodyTag.ActiveLinkKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxBodyTag.BackGroundKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxBodyTag.BackGroundColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ABodyTag: TdxBodyTag;
begin
  ABodyTag := TdxBodyTag(ATag);
  ABodyTag.FBgColor := GetColorValue(AValue);
  ABodyTag.FUseBgColor := True;
end;

class procedure TdxBodyTag.OpenLinkColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxBodyTag.TextColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ABodyTag: TdxBodyTag;
begin
  ABodyTag := TdxBodyTag(ATag);
  ABodyTag.FTextColor := TdxMarkupLanguageColorParser.ParseColor(AValue);
  ABodyTag.FUseTextColor := True;
end;

class procedure TdxBodyTag.LinkColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

function TdxBodyTag.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

procedure TdxBodyTag.ApplyTagProperties;
begin
  if FUseTextColor then
    TdxHtmlImporter(Importer).Position.CharacterFormatting.ForeColor := FTextColor;
end;

function TdxBodyTag.ApplyCssProperties: TdxParagraphFormattingOptions;
var
  AOptions: TdxParagraphFormattingOptions;
  APageBackColor: TdxAlphaColor;
begin
  AOptions := inherited ApplyCssProperties;
  ApplyDocumentModelWebSettings;
  IgnoredMarginPropertiesFromBlockTags;
  APageBackColor := TdxHtmlImporter(Importer).Position.TableProperties.BackgroundColor;
  if (not TdxAlphaColors.IsEmpty(APageBackColor)) and
    TdxHtmlImporter(Importer).Position.TableProperties.UseBackgroundColor then
  begin
    FBgColor := APageBackColor;
    FUseBgColor := True;
  end;
  Result := AOptions;
end;

procedure TdxBodyTag.ApplyDocumentModelWebSettings;
var
  AHtmlSettings: TdxWebSettings;
  AParagraphFormatting: TdxParagraphFormattingBase;
begin
  AHtmlSettings := Importer.DocumentModel.WebSettings;
  AParagraphFormatting := TdxHtmlImporter(Importer).Position.ParagraphFormatting;
  if AParagraphFormatting.Options.UseLeftIndent then
    AHtmlSettings.LeftMargin := AParagraphFormatting.LeftIndent;
  if AParagraphFormatting.Options.UseRightIndent then
    AHtmlSettings.RightMargin := AParagraphFormatting.RightIndent;
  if AParagraphFormatting.Options.UseSpacingBefore then
    AHtmlSettings.TopMargin := AParagraphFormatting.SpacingBefore;
  if AParagraphFormatting.Options.UseSpacingAfter then
    AHtmlSettings.BottomMargin := AParagraphFormatting.SpacingAfter;
end;

procedure TdxBodyTag.OpenTagProcessCore;
begin
  inherited OpenTagProcessCore;
  ApplyPageBackColorProperties;
end;

procedure TdxBodyTag.ApplyPageBackColorProperties;
var
  AProperties: TdxDocumentProperties;
begin
  if TdxAlphaColors.IsEmpty(FBgColor) and not FUseBgColor then
    Exit;

  AProperties := DocumentModel.DocumentProperties;
  AProperties.PageBackColor := FBgColor;
  AProperties.DisplayBackgroundShape := True;
end;


end.
