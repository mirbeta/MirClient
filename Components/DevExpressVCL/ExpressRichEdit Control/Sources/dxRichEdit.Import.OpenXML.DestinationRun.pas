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

unit dxRichEdit.Import.OpenXML.DestinationRun;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxRichEdit.NativeApi,
  dxRichEdit.Utils.Types,
  dxXMLReader,
  dxRichEdit.Options,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.DestinationNumbering,
  dxRichEdit.Import.OpenXML.DestinationParagraph,
  dxRichEdit.Export.OpenXML.WordProcessingMLBaseExporter;

type

  { TdxRunDestination }

  TdxRunDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    class procedure FillElementHandlerTable(AHandlerTable: TdxElementHandlerTable); static;
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnRunProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRun(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnText(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCarriageReturn(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBreak(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTab(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnPicture(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnDrawing(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnFootNoteReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnEndNoteReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnObject(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnComplexFieldMarker(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnFootNoteSelfReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnEndNoteSelfReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCommentReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSeparator(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSymbol(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnNoBreakHyphen(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
    function IsChoiceNamespaceSupported(const ARequeriesNamespaceUri: string): Boolean; override;
  end;

  { TdxCarriageReturnDestination }

  TdxCarriageReturnDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxRunBreakDestination }

  TdxRunBreakDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxRunTabDestination }

  TdxRunTabDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxSymbolDestination }

  TdxSymbolDestination = class(TdxElementDestination)
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

implementation

uses
  Math, Contnrs, dxGenerics,
  dxRichEdit.Types,
  dxRichEdit.Utils.Exceptions,
  dxCharacters,
  dxRichEdit.Utils.NumberParser,
  dxRichEdit.Export.OpenXML,
  dxRichEdit.Import.OpenXML,
  dxRichEdit.Import.OpenXML.DestinationRunProperties,
  dxRichEdit.Import.OpenXML.DestinationText,
  dxRichEdit.Import.OpenXML.DestinationPicture,
  dxRichEdit.Import.OpenXML.DestinationDrawing,
  dxRichEdit.Import.OpenXML.DestinationFootNote,
  dxRichEdit.Import.OpenXML.DestinationEndNote,
  dxRichEdit.Import.OpenXML.DestinationFields,
  dxRichEdit.Import.OpenXML.DestinationComment,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter;

{ TdxRunDestination }

constructor TdxRunDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
begin
  inherited Create(AImporter);
  AImporter.ResetPositionCharacterProperties;
end;

class constructor TdxRunDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxRunDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxRunDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  FillElementHandlerTable(Result);
end;

class procedure TdxRunDestination.FillElementHandlerTable(AHandlerTable: TdxElementHandlerTable);
begin
  AHandlerTable.Add('r', OnRun);
  AHandlerTable.Add('rPr', OnRunProperties);
  AHandlerTable.Add('t', OnText);
  AHandlerTable.Add('noBreakHyphen', OnNoBreakHyphen);
  AHandlerTable.Add('instrText', OnText);
  AHandlerTable.Add('cr', OnCarriageReturn);
  AHandlerTable.Add('br', OnBreak);
  AHandlerTable.Add('tab', OnTab);
  AHandlerTable.Add('pict', OnPicture);
  AHandlerTable.Add('object', OnObject);
  AHandlerTable.Add('fldChar', OnComplexFieldMarker);
  AHandlerTable.Add('drawing', OnDrawing);
  AHandlerTable.Add('footnoteReference', OnFootNoteReference);
  AHandlerTable.Add('endnoteReference', OnEndNoteReference);
  AHandlerTable.Add('footnoteRef', OnFootNoteSelfReference);
  AHandlerTable.Add('endnoteRef', OnEndNoteSelfReference);
  AHandlerTable.Add('commentReference', OnCommentReference);
  AHandlerTable.Add('spr', OnSeparator);
  AHandlerTable.Add('sym', OnSymbol);
end;

function TdxRunDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

function TdxRunDestination.IsChoiceNamespaceSupported(const ARequeriesNamespaceUri: string): Boolean;
begin
  if SameText(ARequeriesNamespaceUri, TdxOpenXmlExporter.WpsNamespace) then
    Exit(True);

  if SameText(ARequeriesNamespaceUri, TdxOpenXmlExporter.DrawingMLPicturePrefix) then
    Exit(True);
  Result := inherited IsChoiceNamespaceSupported(ARequeriesNamespaceUri);
end;

class function TdxRunDestination.OnRunProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
var
  ACharacterFormatting: TdxCharacterFormattingBase;
begin
  ACharacterFormatting := AImporter.Position.CharacterFormatting;
  ACharacterFormatting.ReplaceInfo(AImporter.DocumentModel.Cache.CharacterFormattingInfoCache.DefaultItem,
    TdxCharacterFormattingOptions.Create(TdxCharacterFormattingOptions.MaskUseNone));
  Result := TdxRunPropertiesDestination.Create(AImporter, ACharacterFormatting);
end;

class function TdxRunDestination.OnRun(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateRunDestination;
end;

class function TdxRunDestination.OnText(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTextDestination.Create(AImporter);
end;

class function TdxRunDestination.OnCarriageReturn(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxCarriageReturnDestination.Create(AImporter);
end;

class function TdxRunDestination.OnBreak(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxRunBreakDestination.Create(AImporter);
end;

class function TdxRunDestination.OnTab(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxRunTabDestination.Create(AImporter);
end;

class function TdxRunDestination.OnPicture(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxInlinePictureDestination.Create(AImporter);
end;

class function TdxRunDestination.OnDrawing(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingDestination.Create(AImporter);
end;

class function TdxRunDestination.OnFootNoteReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxFootNoteReferenceDestination.Create(AImporter);
end;

class function TdxRunDestination.OnEndNoteReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxEndNoteReferenceDestination.Create(AImporter);
end;

class function TdxRunDestination.OnObject(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxInlineObjectDestination.Create(AImporter);
end;

class function TdxRunDestination.OnComplexFieldMarker(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxFieldCharDestination.Create(AImporter);
end;

class function TdxRunDestination.OnFootNoteSelfReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
var
  ADestinations: TArray<TdxDestination>;
  ACount, I: Integer;
  ADestination: TdxFootNoteDestination;
begin
  ADestinations := AImporter.DestinationStack.ToArray;
  ACount := Length(ADestinations);
  for I := 0 to ACount - 1 do
  begin
    ADestination := Safe<TdxFootNoteDestination>.Cast(ADestinations[I]);
    if ADestination <> nil then
      Exit(ADestination.OnFootNoteReference(AReader));
  end;
  Result := nil;
end;

class function TdxRunDestination.OnNoBreakHyphen(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  AImporter.PieceTable.InsertTextCore(AImporter.Position, '-');
  Result := nil;
end;

class function TdxRunDestination.OnEndNoteSelfReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
var
  ADestinations: TArray<TdxDestination>;
  ACount, I: Integer;
  ADestination: TdxEndNoteDestination;
begin
  ADestinations := AImporter.DestinationStack.ToArray;
  ACount := Length(ADestinations);
  for I := 0 to ACount - 1 do
  begin
    ADestination := Safe<TdxEndNoteDestination>.Cast(ADestinations[I]);
    if ADestination <> nil then
      Exit(ADestination.OnEndNoteReference(AReader));
  end;
  Result := nil;
end;

class function TdxRunDestination.OnCommentReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxCommentReferenceElementDestination.Create(AImporter);
end;

class function TdxRunDestination.OnSeparator(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSeparatorDestination.Create(AImporter);
end;

class function TdxRunDestination.OnSymbol(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSymbolDestination.Create(AImporter);
end;

{ TdxCarriageReturnDestination }

procedure TdxCarriageReturnDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  case Importer.InnerOptions.LineBreakSubstitute of
    TdxLineBreakSubstitute.Space:
      Importer.PieceTable.InsertTextCore(Importer.Position, TdxCharacters.Space);
    TdxLineBreakSubstitute.Paragraph:
      Importer.PieceTable.InsertParagraphCore(Importer.Position);
    else
      Importer.PieceTable.InsertTextCore(Importer.Position, TdxCharacters.LineBreak);
  end;
end;

{ TdxRunBreakDestination }

procedure TdxRunBreakDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ABreakCharacter: Char;
begin
  ABreakCharacter := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValue<Char>(AReader, 'type',
    TdxOpenXmlExporter.RunBreaksTable, TdxCharacters.LineBreak);
  Importer.PieceTable.InsertTextCore(Importer.Position, ABreakCharacter);
end;

{ TdxRunTabDestination }

procedure TdxRunTabDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ACh: Char;
begin
  if Importer.DocumentModel.DocumentCapabilities.TabSymbolAllowed then
    ACh := TdxCharacters.TabMark
  else
    ACh := TdxCharacters.Space;
  Importer.PieceTable.InsertTextCore(Importer.Position, ACh);
end;

{ TdxSymbolDestination }

function TdxSymbolDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := ElementHandlerTable.Empty;
end;

function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

procedure TdxSymbolDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AFontName, ACode, AOldFontName: string;
  ACharCode: Integer;
  ASymbol: Char;
  ACharacterFormatting: TdxCharacterFormattingBase;
begin
  AFontName := AReader.GetAttribute('font', Importer.WordProcessingNamespaceConst);
  ACode := Trim(AReader.GetAttribute('char', Importer.WordProcessingNamespaceConst));
  if (AFontName = '') or (ACode = '') then
    Exit;

  if not TdxNumber.TryParse(ACode, TdxNumberStyles.HexNumber, ACharCode) then
    Exit;

  if (ACharCode < 0) or (ACharCode > $FFFF) then
    Exit;

  ASymbol := Char(ACharCode);
  ACharacterFormatting := Importer.Position.CharacterFormatting;
  if ACharacterFormatting.Options.UseFontName then
    AOldFontName := ACharacterFormatting.FontName
  else
    AOldFontName := '';
  ACharacterFormatting.FontName := AFontName;
  try
    PieceTable.InsertTextCore(Importer.Position, ASymbol);
  finally
    ACharacterFormatting.ResetUse([TdxUsedCharacterFormattingOption.UseFontName]);
    if AOldFontName <> '' then
      ACharacterFormatting.FontName := AOldFontName;
  end;
end;

end.

