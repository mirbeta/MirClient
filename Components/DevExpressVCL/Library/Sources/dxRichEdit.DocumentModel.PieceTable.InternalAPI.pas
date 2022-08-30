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

unit dxRichEdit.DocumentModel.PieceTable.InternalAPI;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.UnitConverter,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Intervals.Core,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.Tables.Core,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Section;

type
  { TdxInternalAPI }

  TdxInternalAPI = class(TdxCustomInternalAPI, IdxDocumentModelStructureChangedListener, IdxDocumentExporterFactory,
    IdxDocumentImporterFactory)
  strict private
    FCachedHtmlContent: TdxNullableString;
    FCachedMhtContent: TdxNullableString;
    FCachedWordMLContent: TdxNullableString;
    FCachedXamlContent: TdxNullableString;
    FCachedOpenXmlContent: TdxNullableValue<TArray<Byte>>;
    FCachedOpenDocumentContent: TdxNullableValue<TArray<Byte>>;
    FCachedDocContent: TdxNullableValue<TArray<Byte>>;
    FDocumentPositions: TdxList<TdxDocumentModelPositionAnchor>;
    FDocumentModel: TdxDocumentModel;
    FExporterFactory: IdxDocumentExporterFactory;
    FImporterFactory: IdxDocumentImporterFactory;
    FUnitConverters: array[TdxMeasurementUnit] of TdxUnitConverter;
    FOnDocumentReplaced: TdxNotifyEventHandler;
    FOnParagraphInserted: TdxParagraphEventHandler;
    FOnParagraphRemoved: TdxParagraphEventHandler;
    FOnParagraphMerged: TdxParagraphEventHandler;
    FOnSectionInserted: TdxSectionEventHandler;
    FOnSectionRemoved: TdxSectionEventHandler;
    FOnFieldInserted: TdxFieldEventHandler;
    FOnFieldRemoved: TdxFieldEventHandler;
    FOnHyperlinkInfoInserted: TdxHyperlinkInfoEventHandler;
    FOnHyperlinkInfoDeleted: TdxHyperlinkInfoEventHandler;
    procedure RaiseParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex);
    procedure RaiseParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex);
    procedure RaiseParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex);
  strict private
    procedure UpdateAnchors(const AnchorAction: TdxAction<TdxDocumentModelPositionAnchor>);

    function PrepareModelForExport(AFormat: TdxRichEditDocumentFormat): TdxDocumentModel;
    function GetDocumentRtfContentCore(AOptions: TdxRtfDocumentExporterOptions = nil;
      ALastParagraphRunNotSelected: Boolean = False; AKeepFieldCodeViewState: Boolean = False;
      AForceRaiseBeforeExport: Boolean = False; AForceRaiseAfterExport: Boolean = False): TdxChunkedStringBuilder;
    function GetDocumentPlainTextContentCore(AOptions: TdxPlainTextDocumentExporterOptions = nil): TdxChunkedStringBuilder;

    procedure CreateUnitConverters;
    procedure DestroyUnitConverters;
  strict private
    FCachedPlainTextContent: TdxNullableString;
    FCachedRtfContent: TdxNullableString;
  protected
    function GetRtfText: string; override;
    function GetText: string; override;
    procedure SetRtfText(const Value: string); override;
    procedure SetText(const Value: string); override;
    function GetOpenXmlBytes: TArray<Byte>;
    procedure SetOpenXmlBytes(const AValue: TArray<Byte>);
    function GetHtmlText: string; override;
    procedure SetHtmlText(const AValue: string); override;

    function GetExporterFactory: IdxDocumentExporterFactory; override;
    function GetImporterFactory: IdxDocumentImporterFactory; override;
    function GetModified: Boolean; override;
    procedure SetExporterFactory(const Value: IdxDocumentExporterFactory); override;
    procedure SetImporterFactory(const Value: IdxDocumentImporterFactory); override;
    procedure SetModified(const Value: Boolean); override;
    function GetConverter(Index: TdxMeasurementUnit): TdxUnitConverter; override;
    function CreateHtmlExporter(ADocumentModel: TdxDocumentModel; AOptions: TdxDocumentExporterOptions): TObject;
    function CreateHtmlImporter(ADocumentModel: TdxDocumentModel; AOptions: TdxDocumentImporterOptions): TObject;

  {$REGION 'IdxDocumentModelStructureChangedListener'}
    procedure OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
      AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
      AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
    procedure OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
    procedure OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ALength, AHistoryNotificationId: Integer);
    procedure OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ASplitOffset: Integer);
    procedure OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer);
    procedure OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
      ADeltaRunLength: Integer);
    procedure OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
    procedure OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
    procedure OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
    procedure OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
    procedure OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
  {$ENDREGION}

    procedure SuspendProgressIndication;
    procedure ResumeProgressIndication;

    function GetDocumentRtfContentSaveMemory(AOptions: TdxRtfDocumentExporterOptions;
      ALastParagraphRunNotSelected: Boolean = False;
      AForceRaiseBeforeExport: Boolean = False; AForceRaiseAfterExport: Boolean = False): TdxChunkedStringBuilder;
    function GetDocumentPlainTextContentSaveMemory(AOptions: TdxPlainTextDocumentExporterOptions): TdxChunkedStringBuilder;

    procedure LoadDocumentPlainTextContentCore(AStream: TStream; AOptions: TdxPlainTextDocumentImporterOptions); virtual;

    procedure ApplyDefaultOptions(AOptions: TdxDocumentImporterOptions); overload;
    procedure ApplyDefaultOptions(AOptions: TdxDocumentExporterOptions); overload;
    function RemoveParagraphs(const APlainTextContent: string): string; virtual;
    procedure SubscribeDocumentModelEvents;
    procedure UnsubscribeDocumentModelEvents;
    procedure SubscribeDocumentModelEventsSetContentMode;
    procedure UnsubscribeDocumentModelEventsSetContentMode;
    procedure ClearAnchors; override;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;

    procedure RegisterAnchor(APos: TdxDocumentModelPositionAnchor); override;
    procedure UnregisterAnchor(APos: TdxDocumentModelPositionAnchor); override;

    function GetDocumentRtfContent(AOptions: TdxRtfDocumentExporterOptions = nil;
      ALastParagraphRunNotSelected: Boolean = False; AKeepFieldCodeViewState: Boolean = False;
      AForceRaiseBeforeExport: Boolean = False; AForceRaiseAfterExport: Boolean = False): string; override;
    procedure SaveDocumentRtfContent(AStream: TStream; AOptions: TdxRtfDocumentExporterOptions); override;
    procedure SetDocumentRtfContent(const ARtfContent: string); overload;
    procedure SetDocumentRtfContent(const ARtfContent: string; AOptions: TdxRtfDocumentImporterOptions); overload;
    procedure LoadDocumentRtfContent(AStream: TStream; AOptions: TdxRtfDocumentImporterOptions); override;

    function GetDocumentPlainTextContent(AOptions: TdxPlainTextDocumentExporterOptions = nil): string; override;
    procedure SetDocumentPlainTextContent(const APlainTextContent: string); overload;
    procedure SetDocumentPlainTextContent(const APlainTextContent: string;
      AOptions: TdxPlainTextDocumentImporterOptions); overload;
    procedure SetDocumentPlainTextContentCore(const APlainTextContent: string; AChangeType: TdxDocumentModelChangeType);
    procedure SaveDocumentPlainTextContent(AStream: TStream; AOptions: TdxPlainTextDocumentExporterOptions); override;
    procedure LoadDocumentPlainTextContent(AStream: TStream; AOptions: TdxPlainTextDocumentImporterOptions); override;

    function GetDocumentOpenXmlContent(AOptions: TdxOpenXmlDocumentExporterOptions): TArray<Byte>; overload; override;
    function GetDocumentOpenXmlContent: TArray<Byte>; reintroduce; overload;
    procedure SetDocumentOpenXmlContent(const AContent: TArray<Byte>); overload;
    procedure SetDocumentOpenXmlContent(const AContent: TArray<Byte>; AOptions: TdxOpenXmlDocumentImporterOptions); overload;
    procedure LoadDocumentOpenXmlContent(AStream: TStream; AOptions: TdxOpenXmlDocumentImporterOptions); override;
    procedure SaveDocumentOpenXmlContent(AStream: TStream; AOptions: TdxOpenXmlDocumentExporterOptions); override;

    procedure LoadDocumentHtmlContent(AStream: TStream; AOptions: TdxHtmlDocumentImporterOptions); override;
    procedure SaveDocumentHtmlContent(AStream: TStream; AOptions: TdxHtmlDocumentExporterOptions); override;
    function GetDocumentHtmlContent: string; overload;
    function GetDocumentHtmlContent(AOptions: TdxHtmlDocumentExporterOptions): string; overload;
    procedure SetDocumentHtmlContent(const AContent: string); overload;
    procedure SetDocumentHtmlContent(const AContent: string; AOptions: TdxHtmlDocumentImporterOptions); overload;

    function GetDocumentDocContent(AOptions: TdxDocDocumentExporterOptions): TArray<Byte>; overload; override;
    function GetDocumentDocContent: TArray<Byte>; reintroduce; overload;
    procedure SetDocumentDocContent(const AContent: TArray<Byte>); overload;
    procedure SetDocumentDocContent(const AContent: TArray<Byte>; AOptions: TdxDocDocumentImporterOptions); overload;
    procedure LoadDocumentDocContent(AStream: TStream; AOptions: TdxDocDocumentImporterOptions); override;
    procedure SaveDocumentDocContent(AStream: TStream; AOptions: TdxDocDocumentExporterOptions); override;

    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property ExporterFactory: IdxDocumentExporterFactory read GetExporterFactory write SetExporterFactory;
    property ImporterFactory: IdxDocumentImporterFactory read GetImporterFactory write SetImporterFactory;
    property Modified: Boolean read GetModified write SetModified;

    property RtfText: string read GetRtfText write SetRtfText;
    property OpenXmlBytes: TArray<Byte> read GetOpenXmlBytes write SetOpenXmlBytes;
    property HtmlText: string read GetHtmlText write SetHtmlText;
    property Text: string read GetText write SetText;
    procedure RaiseSectionInserted(E: TdxSectionEventArgs);
    procedure RaiseSectionRemoved(E: TdxSectionEventArgs); virtual;
    procedure RaiseDocumentReplaced; virtual;
    procedure RaiseFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
    procedure RaiseFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);

    procedure RaiseHyperlinkInfoInserted(E: TdxHyperlinkInfoEventArgs);
    procedure RaiseHyperlinkInfoDeleted(E: TdxHyperlinkInfoEventArgs);

    procedure ResetCachedContent; virtual;
    procedure SetDocumentContent<T>(AValue: T; const ASetterMethod: TdxAction<T>);

    procedure OnDocumentModelContentChanged(Sender: TObject; E: TdxEventArgs); virtual;
    procedure OnEndDocumentUpdate(Sender: TObject; E: TdxDocumentUpdateCompleteEventArgs); virtual;
    procedure OnEndDocumentUpdateSetContentMode(Sender: TObject; E: TdxDocumentUpdateCompleteEventArgs); virtual;
    procedure OnDocumentReplaced; virtual;
    procedure OnHyperlinkInfoDeleted(Sender: TObject; E: TdxHyperlinkInfoEventArgs);
    procedure OnHyperlinkInfoInserted(Sender: TObject; E: TdxHyperlinkInfoEventArgs);
    procedure CreateNewDocument; override;

    procedure OnSectionInserted(Sender: TObject; E: TdxSectionEventArgs); virtual;
    procedure OnSectionRemoved(Sender: TObject; E: TdxSectionEventArgs); virtual;
    property DocumentReplaced: TdxNotifyEventHandler read FOnDocumentReplaced;
    property FieldInserted: TdxFieldEventHandler read FOnFieldInserted;
    property FieldRemoved: TdxFieldEventHandler read FOnFieldRemoved;
    property HyperlinkInfoInserted: TdxHyperlinkInfoEventHandler read FOnHyperlinkInfoInserted;
    property HyperlinkInfoDeleted: TdxHyperlinkInfoEventHandler read FOnHyperlinkInfoDeleted;
    property ParagraphInserted: TdxParagraphEventHandler read FOnParagraphInserted;
    property ParagraphRemoved: TdxParagraphEventHandler read FOnParagraphRemoved;
    property ParagraphMerged: TdxParagraphEventHandler read FOnParagraphMerged;
    property SectionInserted: TdxSectionEventHandler read FOnSectionInserted;
    property SectionRemoved: TdxSectionEventHandler read FOnSectionRemoved;
  end;

implementation

uses
  Contnrs, StrUtils, dxOLECryptoContainer,
  dxCharacters,
  dxRichEdit.DocumentModel.TextManipulatorHelper,
  dxEncoding,
  dxRichEdit.Utils.ProgressIndication,
  dxRichEdit.Import,
  dxRichEdit.Import.Core,
  dxRichEdit.Import.Formats,
  dxRichEdit.DocumentModel.Exporter,
  dxRichEdit.Export.Core,
  dxRichEdit.Export.Formats;

{ TdxInternalAPI }

procedure TdxInternalAPI.ClearAnchors;
begin
  FDocumentPositions.Clear;
end;

constructor TdxInternalAPI.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
  FDocumentPositions := TdxList<TdxDocumentModelPositionAnchor>.Create;
  CreateUnitConverters;
  SubscribeDocumentModelEvents;
end;

destructor TdxInternalAPI.Destroy;
begin
  UnsubscribeDocumentModelEvents;
  FreeAndNil(FDocumentPositions);
  DestroyUnitConverters;
  inherited Destroy;
end;

procedure TdxInternalAPI.RegisterAnchor(APos: TdxDocumentModelPositionAnchor);
begin
  FDocumentPositions.Add(APos);
end;

function TdxInternalAPI.RemoveParagraphs(
  const APlainTextContent: string): string;
var
  L: Integer;
  AInput, AOutput: PChar;
  C: Char;
begin
  L := Length(APlainTextContent);
  if L = 0 then
    Exit('');
  SetLength(Result, L);
  L := 0;
  AInput := PChar(APlainTextContent);
  AOutput := PChar(Result);
  while AInput^ <> #$0000 do
  begin
    C := AInput^;
    case C of
      TdxCharacters.SectionMark:
        AOutput^ := TdxCharacters.ParagraphMark;
      #13:
        begin
          if AInput[1] = #10 then
            Inc(AInput);
          AOutput^ := TdxCharacters.Space;
        end;
      #10:
        begin
          if AInput[1] = #13 then
            Inc(AInput);
          AOutput^ := TdxCharacters.Space;
        end;
    else
      AOutput^ := C;
    end;
    Inc(AInput);
    Inc(AOutput);
    Inc(L);
  end;
  SetLength(Result, L);
end;

procedure TdxInternalAPI.ResetCachedContent;
begin
  FCachedPlainTextContent.Reset;
  FCachedRtfContent.Reset;
  FCachedHtmlContent.Reset;
  FCachedMhtContent.Reset;
  FCachedWordMLContent.Reset;
  FCachedXamlContent.Reset;
  FCachedOpenXmlContent.Reset;
  FCachedOpenDocumentContent.Reset;
  FCachedDocContent.Reset;
end;

procedure TdxInternalAPI.SetDocumentContent<T>(AValue: T; const ASetterMethod: TdxAction<T>);
begin
  ResetCachedContent;
  UnsubscribeDocumentModelEvents;
  SubscribeDocumentModelEventsSetContentMode;
  try
    SuspendProgressIndication;
    try
      ASetterMethod(AValue);
    finally
      ResumeProgressIndication;
    end;
  finally
    UnsubscribeDocumentModelEventsSetContentMode;
    SubscribeDocumentModelEvents;
  end;
end;

procedure TdxInternalAPI.UnregisterAnchor(APos: TdxDocumentModelPositionAnchor);
var
  I: Integer;
  AAnchor: TdxDocumentModelPositionAnchor;
begin
  for I := FDocumentPositions.Count - 1 downto 0 do
  begin
    AAnchor := FDocumentPositions[I];
    if AAnchor <> nil then
    begin
      if AAnchor.Equals(APos) then
      begin
        FDocumentPositions.Delete(I);
        Break;
      end;
    end
    else
      FDocumentPositions.Delete(I);
  end;
end;

function TdxInternalAPI.GetDocumentRtfContent(AOptions: TdxRtfDocumentExporterOptions = nil;
  ALastParagraphRunNotSelected: Boolean = False;
  AKeepFieldCodeViewState: Boolean = False;
  AForceRaiseBeforeExport: Boolean = False; AForceRaiseAfterExport: Boolean = False): string;
var
  AResult: TdxChunkedStringBuilder;
begin
  AResult := GetDocumentRtfContentCore(AOptions, ALastParagraphRunNotSelected, AKeepFieldCodeViewState, AForceRaiseBeforeExport, AForceRaiseAfterExport);
  try
    Result := AResult.ToString;
  finally
    AResult.Free;
  end;
end;

procedure TdxInternalAPI.SaveDocumentPlainTextContent(AStream: TStream;
  AOptions: TdxPlainTextDocumentExporterOptions);
var
  AContent: TdxChunkedStringBuilder;
  ABytes: TBytes;
  APreamble: TBytes;
begin
  DocumentModel.RaiseBeforeExport(TdxRichEditDocumentFormat.PlainText, AOptions);
  AContent := GetDocumentPlainTextContentSaveMemory(AOptions);
  try
    if AOptions.WriteEncodingPreamble then
    begin
      APreamble := AOptions.ActualEncoding.GetPreamble;
      if Length(APreamble) > 0 then
        AStream.WriteBuffer(APreamble[0], Length(APreamble));
    end;

    ABytes := AContent.GetBytes(AOptions.ActualEncoding);
    AStream.WriteBuffer(ABytes[0], Length(ABytes));
    DocumentModel.RaiseAfterExport;
  finally
    AContent.Free;
  end;
end;

procedure TdxInternalAPI.SaveDocumentRtfContent(AStream: TStream; AOptions: TdxRtfDocumentExporterOptions);
var
  AContent: TdxChunkedStringBuilder;
  ABytes: TBytes;
begin
  DocumentModel.RaiseBeforeExport(TdxRichEditDocumentFormat.Rtf, AOptions);
  AContent := GetDocumentRtfContentSaveMemory(AOptions);
  try
    ABytes := AContent.GetBytes(AOptions.ActualEncoding);
    AStream.WriteBuffer(ABytes[0], Length(ABytes));
    DocumentModel.RaiseAfterExport;
  finally
    AContent.Free;
  end;
end;

procedure TdxInternalAPI.CreateUnitConverters;
begin
  FUnitConverters[TdxMeasurementUnit.Document] := TdxDocumentsToModelUnitsConverter.Create(DocumentModel.UnitConverter);
  FUnitConverters[TdxMeasurementUnit.Inch] := TdxInchesToModelUnitsConverter.Create(DocumentModel.UnitConverter);
  FUnitConverters[TdxMeasurementUnit.Millimeter] := TdxMillimetersToModelUnitsConverter.Create(DocumentModel.UnitConverter);
  FUnitConverters[TdxMeasurementUnit.Centimeter] := TdxCentimetersToModelUnitsConverter.Create(DocumentModel.UnitConverter);
  FUnitConverters[TdxMeasurementUnit.Point] := TdxPointsToModelUnitsConverter.Create(DocumentModel.UnitConverter);
end;

procedure TdxInternalAPI.DestroyUnitConverters;
var
  I: TdxMeasurementUnit;
begin
  for I := Low(TdxMeasurementUnit) to High(TdxMeasurementUnit) do
    FreeAndNil(FUnitConverters[I]);
end;

function TdxInternalAPI.GetConverter(Index: TdxMeasurementUnit): TdxUnitConverter;
begin
  Result := FUnitConverters[Index];
end;

function TdxInternalAPI.GetDocumentPlainTextContent(AOptions: TdxPlainTextDocumentExporterOptions = nil): string;
var
  AResult: TdxChunkedStringBuilder;
begin
  AResult := GetDocumentPlainTextContentCore(AOptions);
  try
    Result := AResult.ToString;
  finally
    AResult.Free;
  end;
end;

function TdxInternalAPI.GetDocumentPlainTextContentSaveMemory(
  AOptions: TdxPlainTextDocumentExporterOptions): TdxChunkedStringBuilder;
begin
  Result := GetDocumentPlainTextContentCore(AOptions);
end;

function TdxInternalAPI.CreateHtmlExporter(ADocumentModel: TdxDocumentModel; AOptions: TdxDocumentExporterOptions): TObject;
begin
  Assert(AOptions is TdxHtmlDocumentExporterOptions);
  Result := TdxExportFileFormats.GetExporter(TdxRichEditDocumentFormat.Html, ADocumentModel, AOptions);
end;

function TdxInternalAPI.CreateHtmlImporter(ADocumentModel: TdxDocumentModel; AOptions: TdxDocumentImporterOptions): TObject;
begin
  Assert(AOptions is TdxHtmlDocumentImporterOptions);
  Result := TdxImportFileFormats.GetImporter(TdxRichEditDocumentFormat.Html, ADocumentModel, AOptions);
end;

procedure TdxInternalAPI.CreateNewDocument;
begin
  DocumentModel.BeginUpdate;
  try
    DocumentModel.DocumentFileName := '';
    DocumentModel.DocumentFormat := TdxRichEditDocumentFormat.Rtf;
    DocumentModel.DocumentSaveOptions.ResetCurrentFileName;
    DocumentModel.DocumentSaveOptions.ResetCurrentFormat;
    SetDocumentPlainTextContentCore('', TdxDocumentModelChangeType.CreateEmptyDocument);
    DocumentModel.SetActivePieceTable(DocumentModel.MainPieceTable, nil);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxInternalAPI.SetDocumentRtfContent(const ARtfContent: string);
var
  AOptions: TdxRtfDocumentImporterOptions;
begin
  AOptions := TdxRtfDocumentImporterOptions.Create;
  try
    ApplyDefaultOptions(AOptions);
    DocumentModel.RaiseBeforeImport(TdxRichEditDocumentFormat.Rtf, AOptions);
    SetDocumentRtfContent(ARtfContent, AOptions);
  finally
    AOptions.Free;
  end;
end;

procedure TdxInternalAPI.SetDocumentRtfContent(const ARtfContent: string; AOptions: TdxRtfDocumentImporterOptions);
var
  ABytes: TArray<Byte>;
  AStream: TBytesStream;
begin
  ABytes := TdxEncoding.ANSI.GetBytes(ARtfContent);
  AStream := TBytesStream.Create(ABytes);
  try
    LoadDocumentRtfContent(AStream, AOptions);
  finally
    AStream.Free;
  end;
end;

procedure TdxInternalAPI.OnParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; ACell: TdxCustomTableCell; AIsParagraphMerged: Boolean;
  AActualParagraphIndex: TdxParagraphIndex; AHistoryNotificationId: Integer);
begin
  UpdateAnchors(procedure(const Sender: TdxDocumentModelPositionAnchor)
    begin
      TdxDocumentModelStructureChangedNotifier.NotifyParagraphInserted(Sender, APieceTable,
        ASectionIndex, AParagraphIndex, ARunIndex, ACell, AIsParagraphMerged,
        AActualParagraphIndex, AHistoryNotificationId);
    end);
  RaiseParagraphInserted(TdxPieceTable(APieceTable), ASectionIndex, AActualParagraphIndex);
end;

procedure TdxInternalAPI.OnParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  UpdateAnchors(procedure(const Sender: TdxDocumentModelPositionAnchor)
    begin
      TdxDocumentModelStructureChangedNotifier.NotifyParagraphRemoved(Sender, APieceTable, ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
    end);
  RaiseParagraphRemoved(APieceTable, ASectionIndex, AParagraphIndex);
end;

procedure TdxInternalAPI.OnParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex; AHistoryNotificationId: Integer);
begin
  UpdateAnchors(procedure(const Sender: TdxDocumentModelPositionAnchor)
    begin
      TdxDocumentModelStructureChangedNotifier.NotifyParagraphMerged(Sender, APieceTable,
        ASectionIndex, AParagraphIndex, ARunIndex, AHistoryNotificationId);
    end);
  RaiseParagraphMerged(APieceTable, ASectionIndex, AParagraphIndex);
end;

procedure TdxInternalAPI.OnRunInserted(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ANewRunIndex: TdxRunIndex; ALength, AHistoryNotificationId: Integer);
begin
  UpdateAnchors(procedure(const Sender: TdxDocumentModelPositionAnchor)
    begin
      TdxDocumentModelStructureChangedNotifier.NotifyRunInserted(Sender, APieceTable, AParagraphIndex, ANewRunIndex, ALength, AHistoryNotificationId);
    end);
end;

procedure TdxInternalAPI.OnRunRemoved(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ALength, AHistoryNotificationId: Integer);
begin
  UpdateAnchors(procedure(const Sender: TdxDocumentModelPositionAnchor)
    begin
      TdxDocumentModelStructureChangedNotifier.NotifyRunRemoved(Sender, APieceTable, AParagraphIndex, ARunIndex, ALength, AHistoryNotificationId);
    end);
end;

procedure TdxInternalAPI.OnRunSplit(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex; ARunIndex: TdxRunIndex;
  ASplitOffset: Integer);
begin
  UpdateAnchors(procedure(const Sender: TdxDocumentModelPositionAnchor)
    begin
      TdxDocumentModelStructureChangedNotifier.NotifyRunSplit(Sender, APieceTable, AParagraphIndex, ARunIndex, ASplitOffset);
    end);
end;

procedure TdxInternalAPI.OnRunJoined(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  AJoinedRunIndex: TdxRunIndex; ASplitOffset, ATailRunLength: Integer);
begin
  UpdateAnchors(procedure(const Sender: TdxDocumentModelPositionAnchor)
    begin
      TdxDocumentModelStructureChangedNotifier.NotifyRunJoined(Sender, APieceTable,
        AParagraphIndex, AJoinedRunIndex, ASplitOffset, ATailRunLength);
    end);
end;

procedure TdxInternalAPI.OnRunMerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  UpdateAnchors(procedure (const AAnchor: TdxDocumentModelPositionAnchor)
    begin
      TdxDocumentModelStructureChangedNotifier.NotifyRunMerged(AAnchor, APieceTable, AParagraphIndex,
        ARunIndex, ADeltaRunLength);
    end);
end;

procedure TdxInternalAPI.OnRunUnmerged(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex; ADeltaRunLength: Integer);
begin
  UpdateAnchors(procedure (const AAnchor: TdxDocumentModelPositionAnchor)
    begin
      TdxDocumentModelStructureChangedNotifier.NotifyRunUnmerged(AAnchor, APieceTable, AParagraphIndex,
        ARunIndex, ADeltaRunLength);
    end);
end;

procedure TdxInternalAPI.OnSectionInserted(Sender: TObject;
  E: TdxSectionEventArgs);
begin
  RaiseSectionInserted(E);
end;

procedure TdxInternalAPI.OnSectionRemoved(Sender: TObject;
  E: TdxSectionEventArgs);
begin
  RaiseSectionRemoved(E);
end;

procedure TdxInternalAPI.OnFieldRemoved(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  RaiseFieldRemoved(APieceTable, AFieldIndex);
end;

procedure TdxInternalAPI.OnFieldInserted(APieceTable: TdxCustomPieceTable; AFieldIndex: Integer);
begin
  RaiseFieldInserted(APieceTable, AFieldIndex);
end;

procedure TdxInternalAPI.OnBeginMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
end;

procedure TdxInternalAPI.OnDocumentModelContentChanged(Sender: TObject;
  E: TdxEventArgs);
begin
  ResetCachedContent;
end;

procedure TdxInternalAPI.OnDocumentReplaced;
begin
  FDocumentPositions.Clear;
  RaiseDocumentReplaced;
end;

procedure TdxInternalAPI.OnHyperlinkInfoDeleted(Sender: TObject; E: TdxHyperlinkInfoEventArgs);
begin
  RaiseHyperlinkInfoDeleted(E);
end;

procedure TdxInternalAPI.OnHyperlinkInfoInserted(Sender: TObject; E: TdxHyperlinkInfoEventArgs);
begin
  RaiseHyperlinkInfoInserted(E);
end;



procedure TdxInternalAPI.OnEndDocumentUpdate(Sender: TObject;
  E: TdxDocumentUpdateCompleteEventArgs);
var
  AChangeActions: TdxDocumentModelChangeActions;
begin
  AChangeActions := E.DeferredChanges.ChangeActions;
  if [TdxDocumentModelChangeAction.RaiseDocumentLoaded, TdxDocumentModelChangeAction.RaiseEmptyDocumentCreated,
      TdxDocumentModelChangeAction.RaiseContentChanged] * AChangeActions <> [] then
    ResetCachedContent;
  if [TdxDocumentModelChangeAction.RaiseDocumentLoaded,
      TdxDocumentModelChangeAction.RaiseEmptyDocumentCreated] * AChangeActions <> [] then
    OnDocumentReplaced;
end;

procedure TdxInternalAPI.OnEndDocumentUpdateSetContentMode(Sender: TObject;
  E: TdxDocumentUpdateCompleteEventArgs);
var
  AChangeActions: TdxDocumentModelChangeActions;
begin
  AChangeActions := E.DeferredChanges.ChangeActions;
  if (TdxDocumentModelChangeAction.RaiseDocumentLoaded in AChangeActions) or
      (TdxDocumentModelChangeAction.RaiseEmptyDocumentCreated in AChangeActions) then
    OnDocumentReplaced;
end;

procedure TdxInternalAPI.OnEndMultipleRunSplit(APieceTable: TdxCustomPieceTable);
begin
  Assert(False, 'not implemented');
end;

procedure TdxInternalAPI.SuspendProgressIndication;
var
  AService: IdxRichEditProgressIndicationService;
begin
  AService := DocumentModel.GetService<IdxRichEditProgressIndicationService>;
  if AService <> nil then
    AService.SuspendProgressIndication;
end;

procedure TdxInternalAPI.ResumeProgressIndication;
var
  AService: IdxRichEditProgressIndicationService;
begin
  AService := DocumentModel.GetService<IdxRichEditProgressIndicationService>;
  if AService <> nil then
    AService.ResumeProgressIndication;
end;

function TdxInternalAPI.GetDocumentRtfContentSaveMemory(AOptions: TdxRtfDocumentExporterOptions;
  ALastParagraphRunNotSelected: Boolean = False;
  AForceRaiseBeforeExport: Boolean = False;
  AForceRaiseAfterExport: Boolean = False): TdxChunkedStringBuilder;
begin
  Result := GetDocumentRtfContentCore(AOptions, ALastParagraphRunNotSelected, False, AForceRaiseBeforeExport, AForceRaiseAfterExport);
end;

function TdxInternalAPI.GetDocumentPlainTextContentCore(AOptions: TdxPlainTextDocumentExporterOptions = nil): TdxChunkedStringBuilder;
var
  ARaiseEvents: Boolean;
  AModelForExport: TdxDocumentModel;
  ANeedDestroyOptions: Boolean;
  AExporter: TdxCustomDocumentModelExporter;
begin
  ARaiseEvents := AOptions = nil;
  ANeedDestroyOptions := AOptions = nil;
  AModelForExport := PrepareModelForExport(TdxRichEditDocumentFormat.PlainText);
  if AOptions = nil then
  begin
    AOptions := TdxPlainTextDocumentExporterOptions.Create;
    ApplyDefaultOptions(AOptions);
  end;
  try
    if ARaiseEvents then
      AModelForExport.RaiseBeforeExport(TdxRichEditDocumentFormat.PlainText, AOptions);
    AExporter := TdxExportFileFormats.GetExporter(TdxRichEditDocumentFormat.PlainText, AModelForExport, AOptions);
    try
      Result := AExporter.ExportSaveMemory;
    finally
      AExporter.Free;
    end;
    if ARaiseEvents then
      AModelForExport.RaiseAfterExport;
  finally
    if ANeedDestroyOptions then
      AOptions.Free;
  end;
end;

procedure TdxInternalAPI.ApplyDefaultOptions(AOptions: TdxDocumentImporterOptions);
var
  ADefaultOptions: TdxDocumentImporterOptions;
begin
  ADefaultOptions := DocumentModel.DocumentImportOptions.GetOptions(AOptions.Format);
  if ADefaultOptions <> nil then
    AOptions.CopyFrom(ADefaultOptions);
end;

procedure TdxInternalAPI.ApplyDefaultOptions(AOptions: TdxDocumentExporterOptions);
var
  ADefaultOptions: TdxDocumentExporterOptions;
begin
  ADefaultOptions := DocumentModel.DocumentExportOptions.GetOptions(AOptions.Format);
  if ADefaultOptions <> nil then
    AOptions.CopyFrom(ADefaultOptions);
end;

procedure TdxInternalAPI.SubscribeDocumentModelEvents;
begin
  DocumentModel.InnerContentChanged.Add(OnDocumentModelContentChanged);
  DocumentModel.BeforeEndDocumentUpdate.Add(OnEndDocumentUpdate);
  DocumentModel.AfterEndDocumentUpdate.Add(OnEndDocumentUpdate);
  DocumentModel.SectionInserted.Add(OnSectionInserted);
  DocumentModel.SectionRemoved.Add(OnSectionRemoved);
  DocumentModel.HyperlinkInfoInserted.Add(OnHyperlinkInfoInserted);
  DocumentModel.HyperlinkInfoDeleted.Add(OnHyperlinkInfoDeleted);
end;

procedure TdxInternalAPI.UnsubscribeDocumentModelEvents;
begin
  DocumentModel.InnerContentChanged.Remove(OnDocumentModelContentChanged);
  DocumentModel.BeforeEndDocumentUpdate.Remove(OnEndDocumentUpdate);
  DocumentModel.AfterEndDocumentUpdate.Remove(OnEndDocumentUpdate);
  DocumentModel.SectionInserted.Remove(OnSectionInserted);
  DocumentModel.SectionRemoved.Remove(OnSectionRemoved);
  DocumentModel.HyperlinkInfoInserted.Remove(OnHyperlinkInfoInserted);
  DocumentModel.HyperlinkInfoDeleted.Remove(OnHyperlinkInfoDeleted);
end;

procedure TdxInternalAPI.SubscribeDocumentModelEventsSetContentMode;
begin
  DocumentModel.BeforeEndDocumentUpdate.Add(OnEndDocumentUpdateSetContentMode);
  DocumentModel.AfterEndDocumentUpdate.Add(OnEndDocumentUpdateSetContentMode);
end;

procedure TdxInternalAPI.UnsubscribeDocumentModelEventsSetContentMode;
begin
  DocumentModel.BeforeEndDocumentUpdate.Remove(OnEndDocumentUpdateSetContentMode);
  DocumentModel.AfterEndDocumentUpdate.Remove(OnEndDocumentUpdateSetContentMode);
end;

procedure TdxInternalAPI.UpdateAnchors(const AnchorAction: TdxAction<TdxDocumentModelPositionAnchor>);
var
  I: Integer;
  AAnchor: TdxDocumentModelPositionAnchor;
begin
  for I := FDocumentPositions.Count - 1 downto 0 do
  begin
    AAnchor := FDocumentPositions[I];
    if AAnchor <> nil then
      AnchorAction(AAnchor)
    else
      FDocumentPositions.Delete(I);
  end;
end;

function TdxInternalAPI.GetExporterFactory: IdxDocumentExporterFactory;
begin
  if FExporterFactory = nil then
    Result := Self
  else
    Result := FExporterFactory;
end;

function TdxInternalAPI.GetImporterFactory: IdxDocumentImporterFactory;
begin
  if FImporterFactory = nil then
    Result := Self
  else
    Result := FImporterFactory;
end;

function TdxInternalAPI.GetModified: Boolean;
begin
  Result := DocumentModel.Modified;
end;

function TdxInternalAPI.GetRtfText: string;
begin
  if FCachedRtfContent.IsNull then
    FCachedRtfContent := GetDocumentRtfContent;
  Result := FCachedRtfContent.Value;
end;

function TdxInternalAPI.GetText: string;
begin
  if FCachedPlainTextContent.IsNull then
    FCachedPlainTextContent := GetDocumentPlainTextContent;
  Result := FCachedPlainTextContent.Value;
end;

procedure TdxInternalAPI.SetRtfText(const Value: string);
begin
  if not FCachedRtfContent.IsNull and (FCachedRtfContent = Value) then
    Exit;
  if Value = '' then
    Text := ''
  else
    SetDocumentContent<string>(Value, SetDocumentRtfContent);
end;

procedure TdxInternalAPI.SetText(const Value: string);
begin
  if not FCachedPlainTextContent.IsNull and (FCachedPlainTextContent = Value) and
      ((Length(FCachedPlainTextContent.Value) > 0) or DocumentModel.IsEmpty) then
    Exit;
  SetDocumentContent<string>(Value, SetDocumentPlainTextContent);
end;

function TdxInternalAPI.GetOpenXmlBytes: TArray<Byte>;
begin
  if FCachedOpenXmlContent.IsNull then
    FCachedOpenXmlContent := GetDocumentOpenXmlContent;
  Result := FCachedOpenXmlContent.Value;
end;

procedure TdxInternalAPI.SetOpenXmlBytes(const AValue: TArray<Byte>);
begin
  if FCachedOpenXmlContent.HasValue and (StringOf(FCachedOpenXmlContent.Value) = StringOf(AValue)) then
    Exit;

  if AValue = nil then
    Text := ''
  else
    SetDocumentContent<TArray<Byte>>(AValue, SetDocumentOpenXmlContent);
end;

function TdxInternalAPI.GetHtmlText: string;
begin
  if FCachedHtmlContent.IsNull then
    FCachedHtmlContent := GetDocumentHtmlContent;
  Result := FCachedHtmlContent;
end;

procedure TdxInternalAPI.SetHtmlText(const AValue: string);
begin
  if FCachedHtmlContent.HasValue and (FCachedHtmlContent = AValue) then
    Exit;

  if AValue = '' then
    Text := ''
  else
    SetDocumentContent<string>(AValue, SetDocumentHtmlContent);
end;

procedure TdxInternalAPI.LoadDocumentPlainTextContent(AStream: TStream;
  AOptions: TdxPlainTextDocumentImporterOptions);
begin
  DocumentModel.RaiseBeforeImport(TdxRichEditDocumentFormat.PlainText, AOptions);
  LoadDocumentPlainTextContentCore(AStream, AOptions);
end;

procedure TdxInternalAPI.LoadDocumentPlainTextContentCore(AStream: TStream;
  AOptions: TdxPlainTextDocumentImporterOptions);
var
  AEncoding: TEncoding;
  ABytes: TArray<Byte>;
  APreambleLength: Integer;
  AContent: string;
begin
  AEncoding := nil;
  if AOptions.AutoDetectEncoding and AOptions.IsDefaultEncoding then
  begin
    AEncoding := TdxEncoding.DetectEncoding(AStream);
    if AEncoding <> nil then
      AOptions.ActualEncoding := AEncoding;
  end;
  if AOptions.ActualEncoding = nil then
    AOptions.ActualEncoding := TEncoding.Default;

  SetLength(ABytes, AStream.Size);
  AStream.Position := 0;
  AStream.Read(ABytes[0], AStream.Size);
  try
    APreambleLength := TEncoding.GetBufferEncoding(ABytes, AEncoding);
    AContent := AOptions.ActualEncoding.GetString(ABytes, APreambleLength, Length(ABytes) - APreambleLength);
    SetDocumentPlainTextContent(AContent, AOptions);
  except
    on E: Exception do
      DocumentModel.RaiseInvalidFormatException(E);
  end;
end;

procedure TdxInternalAPI.LoadDocumentRtfContent(AStream: TStream;
  AOptions: TdxRtfDocumentImporterOptions);
var
  AImporter: TdxDocumentModelImporter;
begin
  DocumentModel.RaiseBeforeImport(TdxRichEditDocumentFormat.Rtf, AOptions);
  DocumentModel.BeginUpdate;
  try
    AImporter := TdxImportFileFormats.GetImporter(TdxRichEditDocumentFormat.Rtf, DocumentModel, AOptions);
    try
      try
        AImporter.Import(AStream);
      except
        on E: Exception do
        begin
          DocumentModel.RaiseInvalidFormatException(E);
          CreateNewDocument;
        end;
      end;
    finally
      AImporter.Free;
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

function TdxInternalAPI.PrepareModelForExport(AFormat: TdxRichEditDocumentFormat): TdxDocumentModel;
begin
  if not DocumentModel.SeparateModelForApiExport then
    Result := DocumentModel
  else
  begin
    if DocumentModel.ModelForExport then
      Result := DocumentModel
    else
      Result := DocumentModel.CreateDocumentModelForExport(nil);
    Result.PreprocessContentBeforeExport(AFormat);
  end;
end;

function TdxInternalAPI.GetDocumentRtfContentCore(AOptions: TdxRtfDocumentExporterOptions = nil;
  ALastParagraphRunNotSelected: Boolean = False; AKeepFieldCodeViewState: Boolean = False;
  AForceRaiseBeforeExport: Boolean = False; AForceRaiseAfterExport: Boolean = False): TdxChunkedStringBuilder;
var
  AModelForExport: TdxDocumentModel;
  AExporter: TdxCustomDocumentModelExporter;
  ANeedDestroyOptions: Boolean;
begin
  AModelForExport := PrepareModelForExport(TdxRichEditDocumentFormat.Rtf);
  AForceRaiseBeforeExport := AForceRaiseBeforeExport or (AOptions = nil);
  AForceRaiseAfterExport := AForceRaiseAfterExport or (AOptions = nil);
  if AOptions = nil then
  begin
    AOptions := TdxRtfDocumentExporterOptions.Create;
    ANeedDestroyOptions := True;
    ApplyDefaultOptions(AOptions);
  end
  else
    ANeedDestroyOptions := False;
  try
    if AForceRaiseBeforeExport then
      AModelForExport.RaiseBeforeExport(TdxRichEditDocumentFormat.Rtf, AOptions);

    AExporter := TdxExportFileFormats.GetExporter(TdxRichEditDocumentFormat.Rtf, DocumentModel, AOptions);
    try
      AExporter.LastParagraphRunNotSelected := ALastParagraphRunNotSelected;
      AExporter.KeepFieldCodeViewState := AKeepFieldCodeViewState;
      Result := AExporter.ExportSaveMemory;
    finally
      AExporter.Free;
    end;
    if AForceRaiseAfterExport then
      AModelForExport.RaiseAfterExport;
  finally
    if ANeedDestroyOptions then
      AOptions.Free;
  end;
end;

procedure TdxInternalAPI.SetDocumentPlainTextContent(
  const APlainTextContent: string);
var
  AOptions: TdxPlainTextDocumentImporterOptions;
begin
  AOptions := TdxPlainTextDocumentImporterOptions.Create;
  try
    ApplyDefaultOptions(AOptions);
    DocumentModel.RaiseBeforeImport(TdxRichEditDocumentFormat.PlainText, AOptions);
    SetDocumentPlainTextContent(APlainTextContent, AOptions);
  finally
    AOptions.Free;
  end;
end;

procedure TdxInternalAPI.SetDocumentPlainTextContent(
  const APlainTextContent: string;
  AOptions: TdxPlainTextDocumentImporterOptions);
begin
  SetDocumentPlainTextContentCore(APlainTextContent, TdxDocumentModelChangeType.LoadNewDocument);
end;

procedure TdxInternalAPI.SetDocumentPlainTextContentCore(
  const APlainTextContent: string; AChangeType: TdxDocumentModelChangeType);
var
  S: string;
begin
  DocumentModel.BeginSetContent;
  try
    if not DocumentModel.DocumentCapabilities.ParagraphsAllowed then
      S := RemoveParagraphs(APlainTextContent)
    else
      S := APlainTextContent;
    TdxTextManipulatorHelper.SetTextCore(DocumentModel.MainPieceTable, S);
  finally
    DocumentModel.EndSetContent(AChangeType, False, nil);
  end;
end;

procedure TdxInternalAPI.LoadDocumentOpenXmlContent(AStream: TStream; AOptions: TdxOpenXmlDocumentImporterOptions);
var
  AImporter: TdxDocumentModelImporter;
begin
  DocumentModel.RaiseBeforeImport(TdxRichEditDocumentFormat.OpenXml, AOptions);
  try
    AImporter := TdxImportFileFormats.GetImporter(TdxRichEditDocumentFormat.OpenXml, DocumentModel, AOptions);
    if AImporter <> nil then
      try
        AImporter.Import(AStream);
      finally
        AImporter.Free;
      end;
  except
    on EdxOLECryptoContainerError do raise;
    on E: Exception do
      begin
        DocumentModel.RaiseInvalidFormatException(E);
        CreateNewDocument;
      end;
  end;
end;

procedure TdxInternalAPI.SaveDocumentOpenXmlContent(AStream: TStream; AOptions: TdxOpenXmlDocumentExporterOptions);
var
  AExporter: TdxCustomDocumentModelExporter;
begin
  DocumentModel.RaiseBeforeExport(TdxRichEditDocumentFormat.OpenXml, AOptions);

  AExporter := TdxExportFileFormats.GetExporter(TdxRichEditDocumentFormat.OpenXml, DocumentModel, AOptions);
  if AExporter <> nil then
    try
      AExporter.Export(AStream);
    finally
      AExporter.Free;
    end;
  DocumentModel.RaiseAfterExport;
end;

function TdxInternalAPI.GetDocumentOpenXmlContent: TArray<Byte>;
begin
  Result := GetDocumentOpenXmlContent(nil);
end;

function TdxInternalAPI.GetDocumentOpenXmlContent(AOptions: TdxOpenXmlDocumentExporterOptions): TArray<Byte>;
var
  AIsDefaultOptions: Boolean;
  AModelForExport: TdxDocumentModel;
  AExporter: TdxCustomDocumentModelExporter;
  AStream: TdxMemoryStream;
  ARaiseEvents: Boolean;
begin
  ARaiseEvents := AOptions = nil;
  AIsDefaultOptions := AOptions = nil;
  AModelForExport := PrepareModelForExport(TdxRichEditDocumentFormat.OpenXml);
  if AOptions = nil then
  begin
    AOptions := TdxOpenXmlDocumentExporterOptions.Create;
    ApplyDefaultOptions(AOptions);
  end;
  if ARaiseEvents then
    AModelForExport.RaiseBeforeExport(TdxRichEditDocumentFormat.OpenXml, AOptions);
  try
    AExporter := TdxExportFileFormats.GetExporter(TdxRichEditDocumentFormat.OpenXml, DocumentModel, AOptions);
    if AExporter <> nil then
    try
      AStream := TdxMemoryStream.Create;
      try
        AExporter.Export(AStream);
        Result := AStream.ToArray;
      finally
        AStream.Free;
      end;
    finally
      AExporter.Free;
    end;
  finally
    if AIsDefaultOptions then
      AOptions.Free;
  end;
  if ARaiseEvents then
    AModelForExport.RaiseAfterExport;
end;

procedure TdxInternalAPI.SetDocumentOpenXmlContent(const AContent: TArray<Byte>);
var
  AOptions: TdxOpenXmlDocumentImporterOptions;
begin
  AOptions := TdxOpenXmlDocumentImporterOptions.Create;
  try
    ApplyDefaultOptions(AOptions);
    DocumentModel.RaiseBeforeImport(TdxRichEditDocumentFormat.OpenXml, AOptions);
    SetDocumentOpenXmlContent(AContent, AOptions);
  finally
    AOptions.Free;
  end;
end;

procedure TdxInternalAPI.SetDocumentOpenXmlContent(const AContent: TArray<Byte>; AOptions: TdxOpenXmlDocumentImporterOptions);
var
  AStream: TdxMemoryStream;
begin
  AStream := TdxMemoryStream.Create(AContent);
  try
    LoadDocumentOpenXmlContent(AStream, AOptions);
  finally
    AStream.Free;
  end;
end;

procedure TdxInternalApi.LoadDocumentHtmlContent(AStream: TStream; AOptions: TdxHtmlDocumentImporterOptions);
var
  AImporter: TdxDocumentModelImporter;
begin
  DocumentModel.RaiseBeforeImport(TdxRichEditDocumentFormat.Html, AOptions);
  try
    AImporter := TdxImportFileFormats.GetImporter(TdxRichEditDocumentFormat.Html, DocumentModel, AOptions);
    if AImporter <> nil then
      try
        AImporter.Import(AStream);
      finally
        AImporter.Free;
      end;
  except
    on E: Exception do
      begin
        DocumentModel.RaiseInvalidFormatException(E);
        CreateNewDocument;
      end;
  end;
end;

procedure TdxInternalApi.SaveDocumentHtmlContent(AStream: TStream; AOptions: TdxHtmlDocumentExporterOptions);
var
  AExporter: TdxCustomDocumentModelExporter;
begin
  DocumentModel.RaiseBeforeExport(TdxRichEditDocumentFormat.Html, AOptions);

  AExporter := TdxExportFileFormats.GetExporter(TdxRichEditDocumentFormat.Html, DocumentModel, AOptions);
  if AExporter <> nil then
    try
      AExporter.Export(AStream);
    finally
      AExporter.Free;
    end;
  DocumentModel.RaiseAfterExport;
end;

function TdxInternalApi.GetDocumentHtmlContent(AOptions: TdxHtmlDocumentExporterOptions): string;
var
  AIsDefaultOptions: Boolean;
  AExporter: TdxCustomDocumentModelExporter;
  AStream: TdxMemoryStream;
  ARaiseEvents: Boolean;
begin
  ARaiseEvents := AOptions = nil;
  AIsDefaultOptions := AOptions = nil;
  if AIsDefaultOptions then
  begin
    AOptions := TdxHtmlDocumentExporterOptions.Create;
    ApplyDefaultOptions(AOptions);
    AOptions.CssPropertiesExportType := TdxCssPropertiesExportType.Style;
    DocumentModel.RaiseBeforeExport(TdxRichEditDocumentFormat.Html, AOptions);
  end;
  if ARaiseEvents then
    DocumentModel.RaiseBeforeExport(TdxRichEditDocumentFormat.Html, AOptions);
  try
    AExporter := TdxExportFileFormats.GetExporter(TdxRichEditDocumentFormat.Html, DocumentModel, AOptions);
    if AExporter <> nil then
    try
      AStream := TdxMemoryStream.Create;
      try
        AExporter.Export(AStream);
        Result := StringOf(AStream.ToArray);
      finally
        AStream.Free;
      end;
    finally
      AExporter.Free;
    end;
  finally
    if AIsDefaultOptions then
      AOptions.Free;
  end;
  if ARaiseEvents then
    DocumentModel.RaiseAfterExport;
end;

function TdxInternalApi.GetDocumentHtmlContent: string;
begin
  Result := GetDocumentHtmlContent(nil);
end;


procedure TdxInternalApi.SetDocumentHtmlContent(const AContent: string);
var
  AOptions: TdxHtmlDocumentImporterOptions;
begin
  AOptions := TdxHtmlDocumentImporterOptions.Create;
  try
    ApplyDefaultOptions(AOptions);
    AOptions.IgnoreMetaCharset := True;
    AOptions.AutoDetectEncoding := False;
    DocumentModel.RaiseBeforeImport(TdxRichEditDocumentFormat.Html, AOptions);
    SetDocumentHtmlContent(AContent, AOptions);
  finally
    AOptions.Free;
  end;
end;

procedure TdxInternalApi.SetDocumentHtmlContent(const AContent: string; AOptions: TdxHtmlDocumentImporterOptions);
var
  ABytes: TArray<Byte>;
  AStream: TdxMemoryStream;
begin
  ABytes := AOptions.ActualEncoding.GetBytes(AContent);
  AStream := TdxMemoryStream.Create(ABytes);
  try
    LoadDocumentHtmlContent(AStream, AOptions);
  finally
    AStream.Free;
  end;
end;

procedure TdxInternalAPI.LoadDocumentDocContent(AStream: TStream; AOptions: TdxDocDocumentImporterOptions);
var
  AImporter: TdxDocumentModelImporter;
begin
  DocumentModel.RaiseBeforeImport(TdxRichEditDocumentFormat.Doc, AOptions);
  try
    AImporter := TdxImportFileFormats.GetImporter(TdxRichEditDocumentFormat.Doc, DocumentModel, AOptions);
    if AImporter <> nil then
      try
        AImporter.Import(AStream);
      finally
        AImporter.Free;
      end;
  except
    on EdxOLECryptoContainerError do raise;
    on E: Exception do
      begin
        DocumentModel.RaiseInvalidFormatException(E);
        CreateNewDocument;
      end;
  end;
end;

procedure TdxInternalAPI.SaveDocumentDocContent(AStream: TStream; AOptions: TdxDocDocumentExporterOptions);
var
  AExporter: TdxCustomDocumentModelExporter;
begin
  DocumentModel.RaiseBeforeExport(TdxRichEditDocumentFormat.Doc, AOptions);

  AExporter := TdxExportFileFormats.GetExporter(TdxRichEditDocumentFormat.Doc, DocumentModel, AOptions);
  if AExporter <> nil then
    try
      AExporter.Export(AStream);
    finally
      AExporter.Free;
    end;
  DocumentModel.RaiseAfterExport;
end;

function TdxInternalAPI.GetDocumentDocContent: TArray<Byte>;
begin
  Result := GetDocumentDocContent(nil);
end;

function TdxInternalAPI.GetDocumentDocContent(AOptions: TdxDocDocumentExporterOptions): TArray<Byte>;
var
  AIsDefaultOptions: Boolean;
  AModelForExport: TdxDocumentModel;
  AExporter: TdxCustomDocumentModelExporter;
  AStream: TdxMemoryStream;
  ARaiseEvents: Boolean;
begin
  ARaiseEvents := AOptions = nil;
  AIsDefaultOptions := AOptions = nil;
  AModelForExport := PrepareModelForExport(TdxRichEditDocumentFormat.Doc);
  if AOptions = nil then
  begin
    AOptions := TdxDocDocumentExporterOptions.Create;
    ApplyDefaultOptions(AOptions);
  end;
  if ARaiseEvents then
    AModelForExport.RaiseBeforeExport(TdxRichEditDocumentFormat.Doc, AOptions);
  try
    AExporter := TdxExportFileFormats.GetExporter(TdxRichEditDocumentFormat.Doc, DocumentModel, AOptions);
    if AExporter <> nil then
    try
      AStream := TdxMemoryStream.Create;
      try
        AExporter.Export(AStream);
        Result := AStream.ToArray;
      finally
        AStream.Free;
      end;
    finally
      AExporter.Free;
    end;
  finally
    if AIsDefaultOptions then
      AOptions.Free;
  end;
  if ARaiseEvents then
    AModelForExport.RaiseAfterExport;
end;

procedure TdxInternalAPI.SetDocumentDocContent(const AContent: TArray<Byte>);
var
  AOptions: TdxDocDocumentImporterOptions;
begin
  AOptions := TdxDocDocumentImporterOptions.Create;
  try
    ApplyDefaultOptions(AOptions);
    DocumentModel.RaiseBeforeImport(TdxRichEditDocumentFormat.Doc, AOptions);
    SetDocumentDocContent(AContent, AOptions);
  finally
    AOptions.Free;
  end;
end;

procedure TdxInternalAPI.SetDocumentDocContent(const AContent: TArray<Byte>; AOptions: TdxDocDocumentImporterOptions);
var
  AStream: TdxMemoryStream;
begin
  AStream := TdxMemoryStream.Create(AContent);
  try
    LoadDocumentDocContent(AStream, AOptions);
  finally
    AStream.Free;
  end;
end;

procedure TdxInternalAPI.SetExporterFactory(const Value: IdxDocumentExporterFactory);
begin
  FExporterFactory := Value;
end;

procedure TdxInternalAPI.SetImporterFactory(const Value: IdxDocumentImporterFactory);
begin
  FImporterFactory := Value;
end;

procedure TdxInternalAPI.SetModified(const Value: Boolean);
begin
  DocumentModel.Modified := Value;
end;

procedure TdxInternalAPI.RaiseDocumentReplaced;
begin
  if not FOnDocumentReplaced.Empty then
    FOnDocumentReplaced.Invoke(Self)
end;

procedure TdxInternalAPI.RaiseFieldInserted(APieceTable: TdxCustomPieceTable;
  AFieldIndex: Integer);
var
  Args: TdxFieldEventArgs;
begin
  if FOnFieldInserted.Empty then
    Exit;
  Args := TdxFieldEventArgs.Create(APieceTable, AFieldIndex);
  try
    FOnFieldInserted.Invoke(Self, Args);
  finally
    Args.Free;
  end;
end;

procedure TdxInternalAPI.RaiseFieldRemoved(APieceTable: TdxCustomPieceTable;
  AFieldIndex: Integer);
var
  Args: TdxFieldEventArgs;
begin
  if FOnFieldRemoved.Empty then
    Exit;
  Args := TdxFieldEventArgs.Create(APieceTable, AFieldIndex);
  try
    FOnFieldRemoved.Invoke(Self, Args);
  finally
    Args.Free;
  end;
end;

procedure TdxInternalAPI.RaiseHyperlinkInfoInserted(E: TdxHyperlinkInfoEventArgs);
begin
  if not FOnHyperlinkInfoInserted.Empty then
    FOnHyperlinkInfoInserted.Invoke(Self, E);
end;

procedure TdxInternalAPI.RaiseHyperlinkInfoDeleted(E: TdxHyperlinkInfoEventArgs);
begin
  if not FOnHyperlinkInfoDeleted.Empty then
    FOnHyperlinkInfoDeleted.Invoke(Self, E);
end;


procedure TdxInternalAPI.RaiseParagraphInserted(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex);
var
  Args: TdxParagraphEventArgs;
begin
  if FOnParagraphInserted.Empty then
    Exit;
  Args := TdxParagraphEventArgs.Create(APieceTable, ASectionIndex, AParagraphIndex);
  try
    FOnParagraphInserted.Invoke(Self, Args);
  finally
    Args.Free;
  end;
end;

procedure TdxInternalAPI.RaiseParagraphRemoved(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex);
var
  Args: TdxParagraphEventArgs;
begin
  if FOnParagraphRemoved.Empty then
    Exit;
  Args := TdxParagraphEventArgs.Create(APieceTable, ASectionIndex, AParagraphIndex);
  try
    FOnParagraphRemoved.Invoke(Self, Args);
  finally
    Args.Free;
  end;
end;

procedure TdxInternalAPI.RaiseSectionInserted(E: TdxSectionEventArgs);
begin
  if not FOnSectionInserted.Empty then
    FOnSectionInserted.Invoke(Self, E);
end;

procedure TdxInternalAPI.RaiseSectionRemoved(E: TdxSectionEventArgs);
begin
  if not FOnSectionRemoved.Empty then
    FOnSectionRemoved.Invoke(Self, E);
end;

procedure TdxInternalAPI.RaiseParagraphMerged(APieceTable: TdxCustomPieceTable; ASectionIndex: TdxSectionIndex;
  AParagraphIndex: TdxParagraphIndex);
var
  Args: TdxParagraphEventArgs;
begin
  if FOnParagraphMerged.Empty then
    Exit;
  Args := TdxParagraphEventArgs.Create(APieceTable, ASectionIndex, AParagraphIndex);
  try
    FOnParagraphMerged.Invoke(Self, Args);
  finally
    Args.Free;
  end;
end;


end.
