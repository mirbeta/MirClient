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

unit dxRichEdit.Export.PlainText;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Graphics, Windows, Generics.Defaults, Generics.Collections,
  dxRichEdit.Options,
  dxRichEdit.NativeApi,
  dxRichEdit.Utils.Types,
  dxCharacters,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.Platform.Font,
  dxRichEdit.Export.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.SectionRange,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Exporter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.NotesRange,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.Export.Formats;

type
  TdxPlainTextExporter = class(TdxDocumentModelExporter)
  strict private
    FSb: TStringBuilder;
    FOptions: TdxPlainTextDocumentExporterOptions;
    function CalculateActualContentLength: Integer;
  protected
    procedure ExportDocument; override;
    procedure ExportTextRun(ARun: TdxTextRun); override;

    function ShouldExportHiddenText: Boolean; override;

    function RichEditExport: string;

    property Content: TStringBuilder read FSb;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxExporterOptions); reintroduce;
    destructor Destroy; override;

    function ExportSaveMemory: TdxChunkedStringBuilder; override;
    procedure ExportFootEndNotes(ANotes: TList<TdxFootNoteExportInfo>; const ASeparator: string);
    procedure ExportFirstPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean); override;
    procedure ExportOddPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean); override;
    procedure ExportEvenPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean); override;
    procedure ExportFirstPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean); override;
    procedure ExportOddPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean); override;
    procedure ExportEvenPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean); override;
    function ExportParagraph(AParagraph: TdxParagraph): TdxParagraphIndex; override;
    procedure ExportParagraphRun(ARun: TdxParagraphRun); override;
    procedure ExportSectionRun(ARun: TdxSectionRun); override;
    procedure ExportFieldCodeStartRun(ARun: TdxFieldCodeStartRun); override;
    procedure ExportFieldCodeEndRun(ARun: TdxFieldCodeEndRun); override;
    procedure ExportFieldResultEndRun(ARun: TdxFieldResultEndRun); override;
    procedure ExportFootNoteRun(ARun: TdxFootNoteRun); override;
    procedure ExportFootNoteRunReference(const AInfo: TdxFootNoteExportInfo; const AFormat: string); virtual;
    procedure ExportEndNoteRun(ARun: TdxEndNoteRun); override;
    procedure Export(AOutputStream: TStream); override;
  end;

  { TdxExportPlainTextFormat }

  TdxExportPlainTextFormat = class(TdxExportFileFormat)
  public
    class function GetDocumentFormat: TdxRichEditDocumentFormat; override;
    function GetExporter(ADocumentModel: TdxCustomDocumentModel;
      const AOptions: IdxExporterOptions): TdxCustomDocumentModelExporter; override;
    function GetDocumentExporter: IdxExporter; override;
  end;

implementation

uses
  dxCore,
  dxRichEdit.Export.PlainText.DocumentExporter,
  dxStringHelper,
  dxRichEdit.DocumentModel.TextManipulatorHelper;

{ TdxPlainTextExporter }

function TdxPlainTextExporter.CalculateActualContentLength: Integer;
var
  ALength: Integer;
  ALastChar: Char;
  ATrail: string;
begin
  ALength := FSb.Length;
  if ALength <= 0 then
    Result := 0
  else
    if ALength = 1 then
    begin
      ALastChar := FSb[ALength - 1];
      if CharInSet(ALastChar, [#13, #10]) then
        Result := 0
      else
        Result := 1;
    end
    else
    begin
      ATrail := FSb.ToString(ALength - 2, 2);
      if (ATrail = #10#13) or (ATrail = #13#10) then
        Result := ALength - 2
      else
        if CharInSet(ATrail[1], [#13, #10]) then
          Result := ALength - 1
        else
          Result := ALength;
    end;
end;

constructor TdxPlainTextExporter.Create(ADocumentModel: TdxCustomDocumentModel;
  const AOptions: IdxExporterOptions);
begin
  inherited Create(ADocumentModel, AOptions);
  FSb := TStringBuilder.Create;
  FOptions := AOptions as TdxPlainTextDocumentExporterOptions;
end;

destructor TdxPlainTextExporter.Destroy;
begin
  FreeAndNil(FSb);
  inherited;
end;

procedure TdxPlainTextExporter.Export(AOutputStream: TStream);
begin
end;

procedure TdxPlainTextExporter.ExportDocument;
begin
  inherited ExportDocument;
  ExportFootEndNotes(FootNoteExportInfos, FOptions.ActualFootNoteSeparator);
  ExportFootEndNotes(EndNoteExportInfos, FOptions.ActualEndNoteSeparator);
end;

procedure TdxPlainTextExporter.ExportFieldCodeEndRun(ARun: TdxFieldCodeEndRun);
begin
  inherited ExportFieldCodeEndRun(ARun);
  if ShouldExportHiddenText and (FOptions.FieldCodeEndMarker <> '') then
    FSb.Append(FOptions.FieldCodeEndMarker);
end;

procedure TdxPlainTextExporter.ExportFieldCodeStartRun(
  ARun: TdxFieldCodeStartRun);
begin
  inherited ExportFieldCodeStartRun(ARun);
  if ShouldExportHiddenText and (FOptions.FieldCodeStartMarker <> '') then
    FSb.Append(FOptions.FieldCodeStartMarker);
end;

procedure TdxPlainTextExporter.ExportFieldResultEndRun(
  ARun: TdxFieldResultEndRun);
begin
  inherited ExportFieldResultEndRun(ARun);
  if ShouldExportHiddenText and (FOptions.FieldResultEndMarker<> '') then
    FSb.Append(FOptions.FieldResultEndMarker);
end;

procedure TdxPlainTextExporter.ExportFirstPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean);
begin
end;

procedure TdxPlainTextExporter.ExportOddPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean);
begin
end;

procedure TdxPlainTextExporter.ExportEvenPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean);
begin
end;

procedure TdxPlainTextExporter.ExportFirstPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean);
begin
end;

procedure TdxPlainTextExporter.ExportOddPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean);
begin
end;

procedure TdxPlainTextExporter.ExportEvenPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean);
begin
end;

function TdxPlainTextExporter.ExportParagraph(
  AParagraph: TdxParagraph): TdxParagraphIndex;
begin
  if FOptions.ExportBulletsAndNumbering and AParagraph.IsInList and AParagraph.ShouldExportNumbering then
    FSb.Append(GetNumberingListText(AParagraph));
  Result := inherited ExportParagraph(AParagraph);
end;

procedure TdxPlainTextExporter.ExportParagraphRun(ARun: TdxParagraphRun);
begin
  FSb.AppendLine;
end;

function TdxPlainTextExporter.ExportSaveMemory: TdxChunkedStringBuilder;
begin
  FSb.Clear;
  inherited Export;
  FSb.Length := CalculateActualContentLength;
  Result := TdxChunkedStringBuilder.Create(FSb.ToString);
end;

procedure TdxPlainTextExporter.ExportFootEndNotes(ANotes: TList<TdxFootNoteExportInfo>; const ASeparator: string);
var
  ACount, I: Integer;
begin
  ACount := ANotes.Count;
  if ACount <= 0 then
    Exit;

  FSb.AppendLine(ASeparator);
  for I := 0 to ACount - 1 do
    PerformExportPieceTable(ANotes[I].Note, ExportPieceTable);
end;

procedure TdxPlainTextExporter.ExportSectionRun(ARun: TdxSectionRun);
begin
  FSb.AppendLine;
end;

procedure TdxPlainTextExporter.ExportTextRun(ARun: TdxTextRun);
var
  AText: string;
  ALines: TArray<string>;
  ALinesCount, I: Integer;
begin
  AText := ARun.GetPlainText(PieceTable.TextBuffer);
  if ARun.AllCaps then
    AText := UpperCase(AText);

  ALines := TdxTextManipulatorHelper.GetPlainTextLines(AText);
  ALinesCount := Length(ALines);
  if ALinesCount > 1 then
    for I := 0 to ALinesCount - 2 do
      FSb.AppendLine(ALines[I]);
  FSb.Append(ALines[ALinesCount - 1]);
end;

function TdxPlainTextExporter.RichEditExport: string;
var
  ASb: TdxChunkedStringBuilder;
begin
  ASb := ExportSaveMemory;
  try
    Result := ASb.ToString;
  finally
    ASb.Free;
  end;
end;

function TdxPlainTextExporter.ShouldExportHiddenText: Boolean;
begin
  Result := FOptions.ExportHiddenText;
end;

procedure TdxPlainTextExporter.ExportFootNoteRun(ARun: TdxFootNoteRun);
var
  AInfo: TdxFootNoteExportInfo;
begin
  if not DocumentModel.DocumentCapabilities.FootNotesAllowed then
    Exit;

  if PieceTable.IsMain then
  begin
    inherited ExportFootNoteRun(ARun);

    AInfo := CreateFootNoteExportInfo(ARun);
    AInfo.Id := FootNoteExportInfos.Count;
    FootNoteExportInfos.Add(AInfo);
    ExportFootNoteRunReference(AInfo, FOptions.ActualFootNoteNumberStringFormat);
  end
  else
  begin
    if FindFootNoteExportInfoByNote(FootNoteExportInfos, PieceTable, AInfo) then
      ExportFootNoteRunReference(AInfo, FOptions.ActualFootNoteNumberStringFormat);
  end;
end;

procedure TdxPlainTextExporter.ExportFootNoteRunReference(const AInfo: TdxFootNoteExportInfo; const AFormat: string);
begin
  FSb.Append(Format(AFormat, [AInfo.NumberText, AInfo.Number, AInfo.Id]));
end;

procedure TdxPlainTextExporter.ExportEndNoteRun(ARun: TdxEndNoteRun);
var
  AInfo: TdxFootNoteExportInfo;
begin
  if not DocumentModel.DocumentCapabilities.EndNotesAllowed then
    Exit;

  if PieceTable.IsMain then
  begin
    inherited ExportEndNoteRun(ARun);

    AInfo := CreateFootNoteExportInfo(ARun);
    AInfo.Id := EndNoteExportInfos.Count;
    EndNoteExportInfos.Add(AInfo);
    ExportFootNoteRunReference(AInfo, FOptions.ActualEndNoteNumberStringFormat);
  end
  else
  begin
    if FindFootNoteExportInfoByNote(EndNoteExportInfos, PieceTable, AInfo) then
      ExportFootNoteRunReference(AInfo, FOptions.ActualEndNoteNumberStringFormat);
  end;
end;

{ TdxExportPlainTextFormat }

function TdxExportPlainTextFormat.GetDocumentExporter: IdxExporter;
begin
  Result := TdxPlainTextDocumentExporter.Create;
end;

class function TdxExportPlainTextFormat.GetDocumentFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.PlainText;
end;

function TdxExportPlainTextFormat.GetExporter(ADocumentModel: TdxCustomDocumentModel;
  const AOptions: IdxExporterOptions): TdxCustomDocumentModelExporter;
begin
  Result := TdxPlainTextExporter.Create(ADocumentModel, AOptions);
end;

end.
