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

unit dxRichEdit.Import.OpenXML.DestinationSection;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxRichEdit.Utils.Types,
  dxXMLReader,
  dxRichEdit.Options,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.Import.OpenXML.DestinationBase;

type
  { TdxSectionDestinationBase }

  TdxSectionDestinationBase = class abstract(TdxElementDestination)
  protected
    class function OnColumns(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnLineNumbering(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnPageNumbering(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnPaperSource(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnMargins(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnPageSize(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTextDirectionOpenXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTextDirectionWordML(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnVerticalAlignment(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSectionStartType(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSectionDifferentFirstPage(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnFootNoteProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnEndNoteProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxSectionDestination }

  TdxSectionDestination = class abstract(TdxSectionDestinationBase)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
  protected
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnSectionHeaderReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSectionFooterReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxColumnsDestination }

  TdxColumnsDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    function GetColumns: TdxSectionColumns;
  protected
    FColumnInfos: TdxColumnInfoCollection;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxColumnsDestination; static;

    property Columns: TdxSectionColumns read GetColumns;
  public
    destructor Destroy; override;
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    class function OnColumn(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxColumnDestination }

  TdxColumnDestination = class(TdxLeafElementDestination)
  strict private
    FColumnInfos: TdxColumnInfoCollection;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AColumnInfos: TdxColumnInfoCollection);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxSectionLineNumberingDestination }

  TdxSectionLineNumberingDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxSectionPageNumberingDestination }

  TdxSectionPageNumberingDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxSectionPaperSourceDestination }

  TdxSectionPaperSourceDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxSectionMarginsDestination }

  TdxSectionMarginsDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxSectionPageSizeDestination }

  TdxSectionPageSizeDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxSectionTextDirectionDestination }

  TdxSectionTextDirectionDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxSectionVerticalAlignmentDestination }

  TdxSectionVerticalAlignmentDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxSectionStartTypeDestination }

  TdxSectionStartTypeDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxSectionDifferentFirstPageDestination }

  TdxSectionDifferentFirstPageDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

implementation

uses
  Math, Contnrs, dxGenerics,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.Platform.Font,
  dxCharacters,
  dxRichEdit.Export.OpenXML,
  dxRichEdit.Export.OpenXML.WordProcessingMLBaseExporter,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter,
  dxRichEdit.Import.OpenXML.DestinationHeaderFooter,
  dxRichEdit.Import.OpenXML.DestinationFootNote,
  dxRichEdit.Import.OpenXML.DestinationEndNote;

{ TdxSectionDestinationBase }

class function TdxSectionDestinationBase.OnColumns(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxColumnsDestination.Create(AImporter);
end;

class function TdxSectionDestinationBase.OnLineNumbering(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSectionLineNumberingDestination.Create(AImporter);
end;

class function TdxSectionDestinationBase.OnPageNumbering(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSectionPageNumberingDestination.Create(AImporter);
end;

class function TdxSectionDestinationBase.OnPaperSource(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSectionPaperSourceDestination.Create(AImporter);
end;

class function TdxSectionDestinationBase.OnMargins(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSectionMarginsDestination.Create(AImporter);
end;

class function TdxSectionDestinationBase.OnPageSize(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSectionPageSizeDestination.Create(AImporter);
end;

class function TdxSectionDestinationBase.OnTextDirectionOpenXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateOpenXmlSectionTextDirectionDestination;
end;

class function TdxSectionDestinationBase.OnTextDirectionWordML(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateWordMLSectionTextDirectionDestination;
end;

class function TdxSectionDestinationBase.OnVerticalAlignment(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSectionVerticalAlignmentDestination.Create(AImporter);
end;

class function TdxSectionDestinationBase.OnSectionStartType(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSectionStartTypeDestination.Create(AImporter);
end;

class function TdxSectionDestinationBase.OnSectionDifferentFirstPage(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSectionDifferentFirstPageDestination.Create(AImporter);
end;

class function TdxSectionDestinationBase.OnFootNoteProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSectionLevelFootNotePropertiesDestination.Create(AImporter, AImporter.CurrentSection.FootNote);
end;

class function TdxSectionDestinationBase.OnEndNoteProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSectionLevelEndNotePropertiesDestination.Create(AImporter, AImporter.CurrentSection.EndNote);
end;

{ TdxSectionDestination }

class constructor TdxSectionDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxSectionDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxSectionDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('cols', OnColumns);
  Result.Add('lnNumType', OnLineNumbering);
  Result.Add('pgNumType', OnPageNumbering);
  Result.Add('paperSrc', OnPaperSource);
  Result.Add('pgMar', OnMargins);
  Result.Add('pgSz', OnPageSize);
  Result.Add('textDirection', OnTextDirectionOpenXml);
  Result.Add('textFlow', OnTextDirectionWordML);
  Result.Add('vAlign', OnVerticalAlignment);
  Result.Add('type', OnSectionStartType);
  Result.Add('headerReference', OnSectionHeaderReference);
  Result.Add('footerReference', OnSectionFooterReference);
  Result.Add('titlePg', OnSectionDifferentFirstPage);
  Result.Add('footnotePr', OnFootNoteProperties);
  Result.Add('endnotePr', OnEndNoteProperties);
end;

function TdxSectionDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxSectionDestination.OnSectionHeaderReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  if AImporter.DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
    Result := TdxHeaderReferenceDestination.Create(AImporter)
  else
    Result := TdxEmptyDestination.Create(AImporter);
end;

class function TdxSectionDestination.OnSectionFooterReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  if AImporter.DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
    Result := TdxFooterReferenceDestination.Create(AImporter)
  else
    Result := TdxEmptyDestination.Create(AImporter);
end;

{ TdxColumnsDestination }

class constructor TdxColumnsDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

destructor TdxColumnsDestination.Destroy;
begin
  FreeAndNil(FColumnInfos);
  inherited Destroy;
end;

class destructor TdxColumnsDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxColumnsDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('col', OnColumn);
end;

function TdxColumnsDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

function TdxColumnsDestination.GetColumns: TdxSectionColumns;
begin
  Result := Importer.CurrentSection.Columns;
end;

class function TdxColumnsDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxColumnsDestination;
begin
  Result := TdxColumnsDestination(AImporter.PeekDestination);
end;

procedure TdxColumnsDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AColumnCount, ASpacing: Integer;
begin
  Columns.EqualWidthColumns := Importer.GetWpSTOnOffValue(AReader, 'equalWidth', False);
  Columns.DrawVerticalSeparator := Importer.GetWpSTOnOffValue(AReader, 'sep', False);
  AColumnCount := Importer.GetWpSTIntegerValue(AReader, 'num', MinInt);
  if AColumnCount > 0 then
    Columns.ColumnCount := AColumnCount;
  ASpacing := Importer.GetWpSTIntegerValue(AReader, 'space', MinInt);
  if ASpacing <> MinInt then
    Columns.Space := UnitConverter.TwipsToModelUnits(ASpacing);

  if not Columns.EqualWidthColumns then
    FColumnInfos := Columns.GetColumns;
end;

procedure TdxColumnsDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  if FColumnInfos <> nil then
  begin
    if FColumnInfos.Count > 0 then
      Columns.SetColumns(FColumnInfos)
    else
      Columns.EqualWidthColumns := True;
    FreeAndNil(FColumnInfos);
  end;
end;

class function TdxColumnsDestination.OnColumn(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
var
  AColumnInfos: TdxColumnInfoCollection;
begin
  AColumnInfos := GetThis(AImporter).FColumnInfos;
  if AColumnInfos <> nil then
    Result := TdxColumnDestination.Create(AImporter, AColumnInfos)
  else
    Result := nil;
end;

{ TdxColumnDestination }

constructor TdxColumnDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AColumnInfos: TdxColumnInfoCollection);
begin
  inherited Create(AImporter);
  Assert(AColumnInfos <> nil);
  FColumnInfos := AColumnInfos;
end;

procedure TdxColumnDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AColumnInfo: TdxColumnInfo;
begin
  AColumnInfo := TdxColumnInfo.Create;
  AColumnInfo.Width := Importer.GetWpSTIntegerValue(AReader, 'w', MinInt);
  if AColumnInfo.Width <= 0 then
    Exit;

  AColumnInfo.Width := UnitConverter.TwipsToModelUnits(AColumnInfo.Width);

  AColumnInfo.Space := Importer.GetWpSTIntegerValue(AReader, 'space', MinInt);
  AColumnInfo.Space := Max(0, UnitConverter.TwipsToModelUnits(AColumnInfo.Space));

  FColumnInfos.Add(AColumnInfo);
end;

{ TdxSectionLineNumberingDestination }

procedure TdxSectionLineNumberingDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ALineNumbering: TdxSectionLineNumbering;
  ACountByAttribute: TdxWordProcessingMLValue;
begin
  ALineNumbering := Importer.CurrentSection.LineNumbering;
  ALineNumbering.StartingLineNumber := Max(1, Importer.GetWpSTIntegerValue(AReader, 'start', MinInt));
  ACountByAttribute := TdxWordProcessingMLValue.Create('countBy', 'count-by');
  ALineNumbering.Step := Max(0, Importer.GetWpSTIntegerValue(AReader, Importer.GetWordProcessingMLValue(ACountByAttribute), MinInt));
  ALineNumbering.Distance := UnitConverter.TwipsToModelUnits(Math.Max(0, Importer.GetWpSTIntegerValue(AReader, 'distance', MinInt)));
  ALineNumbering.NumberingRestartType := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValue<TdxLineNumberingRestart>(
    AReader, 'restart', TdxOpenXmlExporter.LineNumberingRestartTable, TdxLineNumberingRestart.NewPage);
end;

{ TdxSectionPageNumberingDestination }

procedure TdxSectionPageNumberingDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  APageNumbering: TdxSectionPageNumbering;
  AFirstPageNumber: Integer;
  AChapSepAttribute: TdxWordProcessingMLValue;
begin
  APageNumbering := Importer.CurrentSection.PageNumbering;
  AFirstPageNumber := Importer.GetWpSTIntegerValue(AReader, 'start', MinInt);
  if AFirstPageNumber <> MinInt then
  begin
    APageNumbering.FirstPageNumber := Math.Max(0, AFirstPageNumber);
    APageNumbering.ContinueNumbering := False;
  end;
  APageNumbering.NumberingFormat := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValue<TdxNumberingFormat>(AReader,
    'fmt', TdxOpenXmlExporter.PageNumberingFormatTable, TdxNumberingFormat.Decimal);
  AChapSepAttribute := TdxWordProcessingMLValue.Create('chapSep', 'chap-sep');
  APageNumbering.ChapterSeparator := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValue<Char>(AReader,
    Importer.GetWordProcessingMLValue(AChapSepAttribute), TdxOpenXmlExporter.ChapterSeparatorsTable, '.');
end;

{ TdxSectionPaperSourceDestination }

procedure TdxSectionPaperSourceDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ASettings: TdxSectionGeneralSettings;
begin
  ASettings := Importer.CurrentSection.GeneralSettings;
  ASettings.FirstPagePaperSource := Max(1, Importer.GetWpSTIntegerValue(AReader, 'first', MinInt));
  ASettings.OtherPagePaperSource := Max(1, Importer.GetWpSTIntegerValue(AReader, 'other', MinInt));
end;

{ TdxSectionMarginsDestination }

procedure TdxSectionMarginsDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: Integer;
  AMargins: TdxSectionMargins;
begin
  AMargins := Importer.CurrentSection.Margins;
  AValue := Importer.GetWpSTIntegerValue(AReader, 'left', MinInt);
  if AValue <> MinInt then
    AMargins.Left := UnitConverter.TwipsToModelUnits(AValue);
  AValue := Importer.GetWpSTIntegerValue(AReader, 'right', MinInt);
  if AValue <> MinInt then
    AMargins.Right := UnitConverter.TwipsToModelUnits(AValue);
  AValue := Importer.GetWpSTIntegerValue(AReader, 'top', MinInt);
  if AValue <> MinInt then
    AMargins.Top := UnitConverter.TwipsToModelUnits(AValue);
  AValue := Importer.GetWpSTIntegerValue(AReader, 'bottom', MinInt);
  if AValue <> MinInt then
    AMargins.Bottom := UnitConverter.TwipsToModelUnits(AValue);
  AValue := Importer.GetWpSTIntegerValue(AReader, 'header', MinInt);
  if AValue <> MinInt then
    AMargins.HeaderOffset := UnitConverter.TwipsToModelUnits(AValue);
  AValue := Importer.GetWpSTIntegerValue(AReader, 'footer', MinInt);
  if AValue <> MinInt then
    AMargins.FooterOffset := UnitConverter.TwipsToModelUnits(AValue);
  AValue := Importer.GetWpSTIntegerValue(AReader, 'gutter', MinInt);
  if AValue <> MinInt then
    AMargins.Gutter := UnitConverter.TwipsToModelUnits(AValue);
end;

{ TdxSectionPageSizeDestination }

procedure TdxSectionPageSizeDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: Integer;
  APageInfo: TdxPageInfo;
  AOrientation: string;
begin
  APageInfo := TdxPageInfo.Create;
  try
    AValue := Importer.GetWpSTIntegerValue(AReader, 'w', MinInt);
    if AValue <> MinInt then
      APageInfo.Width := UnitConverter.TwipsToModelUnits(AValue);
    AValue := Importer.GetWpSTIntegerValue(AReader, 'h', MinInt);
    if AValue <> MinInt then
      APageInfo.Height := UnitConverter.TwipsToModelUnits(AValue);
    AValue := Importer.GetWpSTIntegerValue(AReader, 'code', MinInt);
    if AValue <> MinInt then
      APageInfo.PaperKind := TdxPaperKind(AValue);

    AOrientation := AReader.GetAttribute('orient', Importer.WordProcessingNamespaceConst);
    APageInfo.Landscape := (AOrientation = 'landscape');
    APageInfo.ValidatePaperKind(DocumentModel.UnitConverter);
    Importer.CurrentSection.Page.CopyFrom(APageInfo);
  finally
    APageInfo.Free;
  end;
end;

{ TdxSectionTextDirectionDestination }

procedure TdxSectionTextDirectionDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Importer.CurrentSection.GeneralSettings.TextDirection := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValue<TdxTextDirection>(
    AReader, 'val', TdxOpenXmlExporter.TextDirectionTable, TdxTextDirection.LeftToRightTopToBottom);
end;

{ TdxSectionVerticalAlignmentDestination }

procedure TdxSectionVerticalAlignmentDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Importer.CurrentSection.GeneralSettings.VerticalTextAlignment := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValue<TdxVerticalAlignment>(
    AReader, 'val', TdxOpenXmlExporter.VerticalAlignmentTable, TdxVerticalAlignment.Top);
end;

{ TdxSectionStartTypeDestination }

procedure TdxSectionStartTypeDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Importer.CurrentSection.GeneralSettings.StartType := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValue<TdxSectionStartType>(
    AReader, 'val', TdxOpenXmlExporter.SectionStartTypeTable, TdxSectionStartType.NextPage);
end;

{ TdxSectionDifferentFirstPageDestination }

procedure TdxSectionDifferentFirstPageDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Importer.CurrentSection.GeneralSettings.DifferentFirstPage := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

end.

