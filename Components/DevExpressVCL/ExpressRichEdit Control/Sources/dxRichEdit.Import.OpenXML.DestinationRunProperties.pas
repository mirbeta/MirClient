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

unit dxRichEdit.Import.OpenXML.DestinationRunProperties;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics, dxCultureInfo,

  dxRichEdit.Utils.Types,
  dxXMLReader,
  dxRichEdit.Options,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.Import.OpenXML.DestinationBase;

type

  { TdxRunPropertiesBaseDestination }

  TdxRunPropertiesBaseDestination = class abstract(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FCharacterProperties: IdxCharacterProperties;
  protected
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetCharacterProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): IdxCharacterProperties; static;
    class function OnBold(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnItalic(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCaps(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnHiddenText(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnUnderline(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSingleStrikeThrough(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnDoubleStrikeThrough(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnForeColor(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBackColor(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnShading(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnFontSize(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnScript(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnFontName(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnLanguage(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnNoProof(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const ACharacterProperties: IdxCharacterProperties);

    property CharacterProperties: IdxCharacterProperties read FCharacterProperties;
  end;

  { TdxRunPropertiesDestination }

  TdxRunPropertiesDestination = class(TdxRunPropertiesBaseDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ACharacterFormatting: TdxCharacterFormattingBase);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    class function OnStyle(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxCharacterFormattingLeafElementDestination }

  TdxCharacterFormattingLeafElementDestination = class abstract(TdxLeafElementDestination)
  strict private
    FCharacterProperties: IdxCharacterProperties;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const ACharacterProperties: IdxCharacterProperties);

    property CharacterProperties: IdxCharacterProperties read FCharacterProperties;
  end;

  { TdxBoldDestination }

  TdxBoldDestination = class(TdxCharacterFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxItalicDestination }

  TdxItalicDestination = class(TdxCharacterFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxAllCapsDestination }

  TdxAllCapsDestination = class(TdxCharacterFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxHiddenTextDestination }

  TdxHiddenTextDestination = class(TdxCharacterFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxUnderlineDestination }

  TdxUnderlineDestination = class(TdxCharacterFormattingLeafElementDestination)
  protected
    procedure ImportUnderlineType(AReader: TdxXmlReader);
    procedure ImportUnderlineColor(AReader: TdxXmlReader);
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxSingleStrikeThroughDestination }

  TdxSingleStrikeThroughDestination = class(TdxCharacterFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDoubleStrikeThroughDestination }

  TdxDoubleStrikeThroughDestination = class(TdxCharacterFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxForeColorDestination }

  TdxForeColorDestination = class(TdxCharacterFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxBackColorDestination }

  TdxBackColorDestination = class(TdxCharacterFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxShadingDestination }

  TdxShadingDestination = class(TdxCharacterFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxFontSizeDestination }

  TdxFontSizeDestination = class(TdxCharacterFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxFontScriptDestination }

  TdxFontScriptDestination = class(TdxCharacterFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxFontNameDestination }

  TdxFontNameDestination = class(TdxCharacterFormattingLeafElementDestination)
  protected
    function ReadFontName(AReader: TdxXmlReader): string; virtual;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDefaultFontNameDestination }

  TdxDefaultFontNameDestination = class(TdxFontNameDestination)
  protected
    function ReadFontName(AReader: TdxXmlReader): string; override;
  end;

  { TdxLanguageDestination }

  TdxLanguageDestination = class(TdxCharacterFormattingLeafElementDestination)
  private
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxNoProofDestination }

  TdxNoProofDestination = class(TdxCharacterFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxRunStyleReferenceBaseDestination }

  TdxRunStyleReferenceBaseDestination = class abstract(TdxLeafElementDestination)
  protected
    procedure AssignCharacterStyleIndex(AValue: Integer); virtual; abstract;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    function LookupStyleIndex(const AValue: string): Integer;
  end;

  { TdxRunStyleReferenceDestination }

  TdxRunStyleReferenceDestination = class(TdxRunStyleReferenceBaseDestination)
  protected
    procedure AssignCharacterStyleIndex(AValue: Integer); override;
  end;

implementation

uses
  Math, Contnrs, dxGenerics,
  dxRichEdit.Platform.Font,
  dxCharacters,
  dxRichEdit.Utils.NumberParser,
  dxRichEdit.Export.OpenXML,
  dxRichEdit.Export.OpenXML.WordProcessingMLBaseExporter,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter;

{ TdxRunPropertiesBaseDestination }

constructor TdxRunPropertiesBaseDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const ACharacterProperties: IdxCharacterProperties);
begin
  inherited Create(AImporter);
  Assert(ACharacterProperties <> nil);
  FCharacterProperties := ACharacterProperties;
end;

class constructor TdxRunPropertiesBaseDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxRunPropertiesBaseDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxRunPropertiesBaseDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('b', OnBold);
  Result.Add('i', OnItalic);
  Result.Add('caps', OnCaps);
  Result.Add('vanish', OnHiddenText);
  Result.Add('color', OnForeColor);
  Result.Add('highlight', OnBackColor);
  Result.Add('shd', OnShading);
  Result.Add('strike', OnSingleStrikeThrough);
  Result.Add('dstrike', OnDoubleStrikeThrough);
  Result.Add('u', OnUnderline);
  Result.Add('sz', OnFontSize);
  Result.Add('vertAlign', OnScript);
  Result.Add('rFonts', OnFontName);
  Result.Add('lang', OnLanguage);
  Result.Add('noProof', OnNoProof);
end;

function TdxRunPropertiesBaseDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxRunPropertiesBaseDestination.GetCharacterProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): IdxCharacterProperties;
var
  AThisObject: TdxRunPropertiesBaseDestination;
begin
  AThisObject := TdxRunPropertiesBaseDestination(AImporter.PeekDestination);
  Result := AThisObject.characterProperties;
end;

class function TdxRunPropertiesBaseDestination.OnBold(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxBoldDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

class function TdxRunPropertiesBaseDestination.OnItalic(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxItalicDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

class function TdxRunPropertiesBaseDestination.OnCaps(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxAllCapsDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

class function TdxRunPropertiesBaseDestination.OnHiddenText(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxHiddenTextDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

class function TdxRunPropertiesBaseDestination.OnUnderline(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxUnderlineDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

class function TdxRunPropertiesBaseDestination.OnSingleStrikeThrough(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSingleStrikeThroughDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

class function TdxRunPropertiesBaseDestination.OnDoubleStrikeThrough(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDoubleStrikeThroughDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

class function TdxRunPropertiesBaseDestination.OnForeColor(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxForeColorDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

class function TdxRunPropertiesBaseDestination.OnBackColor(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxBackColorDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

class function TdxRunPropertiesBaseDestination.OnShading(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxShadingDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

class function TdxRunPropertiesBaseDestination.OnFontSize(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxFontSizeDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

class function TdxRunPropertiesBaseDestination.OnScript(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxFontScriptDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

class function TdxRunPropertiesBaseDestination.OnFontName(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxFontNameDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

class function TdxRunPropertiesBaseDestination.OnLanguage(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxLanguageDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

class function TdxRunPropertiesBaseDestination.OnNoProof(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxNoProofDestination.Create(AImporter, GetCharacterProperties(AImporter));
end;

{ TdxRunPropertiesDestination }

constructor TdxRunPropertiesDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ACharacterFormatting: TdxCharacterFormattingBase);
begin
  inherited Create(AImporter, ACharacterFormatting);
  AImporter.Position.CharacterStyleIndex := 0;
end;

class constructor TdxRunPropertiesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxRunPropertiesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxRunPropertiesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxRunPropertiesBaseDestination.CreateElementHandlerTable;
  Result.Add('rStyle', OnStyle);
end;

function TdxRunPropertiesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxRunPropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ACharacterFormatting: TdxCharacterFormattingBase;
begin
  ACharacterFormatting := TdxCharacterFormattingBase(CharacterProperties);
  ACharacterFormatting.BeginUpdate;
end;

procedure TdxRunPropertiesDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  ACharacterFormatting: TdxCharacterFormattingBase;
begin
  ACharacterFormatting := TdxCharacterFormattingBase(CharacterProperties);
  ACharacterFormatting.EndUpdate;
end;

class function TdxRunPropertiesDestination.OnStyle(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxRunStyleReferenceDestination.Create(AImporter);
end;

{ TdxCharacterFormattingLeafElementDestination }

constructor TdxCharacterFormattingLeafElementDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  const ACharacterProperties: IdxCharacterProperties);
begin
  inherited Create(AImporter);
  Assert(Assigned(ACharacterProperties));
  FCharacterProperties := ACharacterProperties;
end;

{ TdxBoldDestination }

procedure TdxBoldDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  CharacterProperties.FontBold := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxItalicDestination }

procedure TdxItalicDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  CharacterProperties.FontItalic := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxAllCapsDestination }

procedure TdxAllCapsDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  CharacterProperties.AllCaps := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxHiddenTextDestination }

procedure TdxHiddenTextDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  CharacterProperties.Hidden := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxUnderlineDestination }

procedure TdxUnderlineDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  ImportUnderlineType(AReader);
  ImportUnderlineColor(AReader);
end;

procedure TdxUnderlineDestination.ImportUnderlineType(AReader: TdxXmlReader);
var
  AValue: string;
begin
  AValue := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
  if AValue <> '' then
  begin
    if AValue = 'words' then
    begin
      CharacterProperties.FontUnderlineType := TdxUnderlineType.Single;
      CharacterProperties.UnderlineWordsOnly := True;
    end
    else
      CharacterProperties.FontUnderlineType := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValueCore<TdxUnderlineType>(AValue,
        TdxOpenXmlExporter.UnderlineTable, TdxUnderlineType.Single);
  end
  else
    CharacterProperties.FontUnderlineType := TdxUnderlineType.None;
end;

procedure TdxUnderlineDestination.ImportUnderlineColor(AReader: TdxXmlReader);
var
  AColor: TdxAlphaColor;
begin
  AColor := TdxWordProcessingMLBaseImporter(Importer).GetWpSTColorValue(AReader, 'color');
  if AColor <> TdxAlphaColors.Empty then
    CharacterProperties.UnderlineColor := AColor;
end;

{ TdxSingleStrikeThroughDestination }

procedure TdxSingleStrikeThroughDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: Boolean;
begin
  AValue := Importer.GetWpSTOnOffValue(AReader, 'val');
  if AValue then
    CharacterProperties.FontStrikeoutType := TdxStrikeoutType.Single
  else
    CharacterProperties.FontStrikeoutType := TdxStrikeoutType.None;
end;

{ TdxDoubleStrikeThroughDestination }

procedure TdxDoubleStrikeThroughDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: Boolean;
begin
  AValue := Importer.GetWpSTOnOffValue(AReader, 'val');
  if AValue then
    CharacterProperties.FontStrikeoutType := TdxStrikeoutType.Double
  else
    CharacterProperties.FontStrikeoutType := TdxStrikeoutType.None;
end;

{ TdxForeColorDestination }

procedure TdxForeColorDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: string;
  AColor: TdxAlphaColor;
begin
  AValue := Importer.ReadAttribute(AReader, 'val');
  if AValue = 'auto' then
    CharacterProperties.ForeColor := TdxAlphaColors.Empty
  else
  begin
    AColor := TdxWordProcessingMLBaseImporter(Importer).GetWpSTColorValue(AReader, 'val');
    if AColor <> TdxAlphaColors.Empty then
      CharacterProperties.ForeColor := AColor;
  end;
end;

{ TdxBackColorDestination }

procedure TdxBackColorDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: string;
  AColor: TdxAlphaColor;
begin
  AValue := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
  if AValue <> '' then
  begin
    AColor := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValueCore<TdxAlphaColor>(AValue,
      TdxOpenXmlExporter.PredefinedBackgroundColors, TdxAlphaColors.Empty);
    if AColor <> TdxAlphaColors.Empty then
      CharacterProperties.BackColor := AColor;
  end;
end;

{ TdxShadingDestination }

procedure TdxShadingDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AColor: TdxAlphaColor;
  AValue: string;
begin
  AColor := TdxWordProcessingMLBaseImporter(Importer).GetWpSTColorValue(AReader, 'fill');
  if AColor <> TdxAlphaColors.Empty then
  begin
    AValue := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
    if (AValue = '') or (AValue = 'nil') or (AValue = 'clear') then
    begin
      if TdxAlphaColors.IsTransparentOrEmpty(CharacterProperties.BackColor) then
        CharacterProperties.BackColor := AColor;
    end;
  end;
end;

{ TdxFontSizeDestination }

procedure TdxFontSizeDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: Integer;
begin
  AValue := Importer.GetWpSTIntegerValue(AReader, 'val', -1);
  if AValue > 0 then
    CharacterProperties.DoubleFontSize := Max(1, AValue);
end;

{ TdxFontScriptDestination }

procedure TdxFontScriptDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: string;
begin
  AValue := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
  if AValue = 'baseline' then
    CharacterProperties.Script := TdxCharacterFormattingScript.Normal
  else
    if AValue = 'subscript' then
      CharacterProperties.Script := TdxCharacterFormattingScript.Subscript
    else
      if AValue = 'superscript' then
        CharacterProperties.Script := TdxCharacterFormattingScript.Superscript;
end;

{ TdxFontNameDestination }

procedure TdxFontNameDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AFontName: string;
begin
  AFontName := ReadFontName(AReader);
  if AFontName <> '' then
    CharacterProperties.FontName := AFontName;
end;

function TdxFontNameDestination.ReadFontName(AReader: TdxXmlReader): string;
var
  AAttribute: TdxWordProcessingMLValue;
begin
  Result := AReader.GetAttribute('ascii', Importer.WordProcessingNamespaceConst);
  if Result <> '' then
    Exit;
  AAttribute := TdxWordProcessingMLValue.Create('hAnsi', 'h-ansi');
  Result := AReader.GetAttribute(Importer.GetWordProcessingMLValue(AAttribute), Importer.WordProcessingNamespaceConst);
end;

{ TdxDefaultFontNameDestination }

function TdxDefaultFontNameDestination.ReadFontName(AReader: TdxXmlReader): string;
var
  AAttribute: TdxWordProcessingMLValue;
begin
  Result := AReader.GetAttribute('ascii', Importer.WordProcessingNamespaceConst);
  if Result <> '' then
    Exit;
  AAttribute := TdxWordProcessingMLValue.Create('hAnsi', 'h-ansi');
  Result := AReader.GetAttribute(Importer.GetWordProcessingMLValue(AAttribute), Importer.WordProcessingNamespaceConst);
  if Result = '' then
    Result := TdxRichEditControlCompatibility.DefaultFontName;
end;

{ TdxLanguageDestination }

procedure TdxLanguageDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
end;



{ TdxNoProofDestination }

procedure TdxNoProofDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  CharacterProperties.NoProof := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxRunStyleReferenceBaseDestination }

procedure TdxRunStyleReferenceBaseDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: string;
  AStyleIndex: Integer;
begin
  AValue := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
  if AValue <> '' then
  begin
    AStyleIndex := LookupStyleIndex(AValue);
    if AStyleIndex >= 0 then
      AssignCharacterStyleIndex(AStyleIndex);
  end;
end;

function TdxRunStyleReferenceBaseDestination.LookupStyleIndex(const AValue: string): Integer;
begin
  Result := Importer.LookupCharacterStyleIndex(AValue);
end;

{ TdxRunStyleReferenceDestination }

procedure TdxRunStyleReferenceDestination.AssignCharacterStyleIndex(AValue: Integer);
begin
  if Importer.DocumentModel.DocumentCapabilities.CharacterStyleAllowed then
    Importer.Position.CharacterStyleIndex := AValue;
end;

end.

