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

unit dxRichEdit.InternalRichEditDocumentServer;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Rtti,
  dxCore, dxCoreClasses,

  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.InnerControl,
  dxRichEdit.ServiceManager,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentModel.RichEditDocumentServer,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.Export.Core,
  dxRichEdit.Import.Core,
  dxRichEdit.View.Core,
  dxRichEdit.Api.Layout.Painters;

type
  TdxInternalRichEditDocumentServer = class;

  { IdxDocumentLayoutService }

  IdxDocumentLayoutService = interface
  ['{FC5FFC24-5A0E-4A3E-A7EA-A2C8F52F348A}']
    function CalculateDocumentLayout: TdxDocumentLayout;
    function CreateService(ADocumentModel: TdxDocumentModel): IdxDocumentLayoutService;
    procedure RemoveService(ADocumentModel: TdxDocumentModel);
    procedure ResetLayout;
  end;

  { IdxInternalRichEditDocumentServerOwner }

  IdxInternalRichEditDocumentServerOwner = interface
  ['{C2A43599-9805-4B3F-8BDE-878568AF1793}']
    function GetInternalServer: TdxInternalRichEditDocumentServer;
    property InternalServer: TdxInternalRichEditDocumentServer read GetInternalServer;
  end;

  { TdxInternalRichEditDocumentServer }

  TdxInternalRichEditDocumentServer = class abstract(TInterfacedObject,
    IdxRichEditDocumentContainer,
    IdxRichEditDocumentServer,
    IdxServiceProvider,
    IdxServiceContainer,
    IdxInnerRichEditDocumentContainerOwner,
    IdxInnerRichEditDocumentServerOwner,
    IdxRichEditDocumentLayoutProvider)
  strict private
    FInnerServer: TdxInnerRichEditDocumentServer;
    function GetBatchUpdateHelper: TdxBatchUpdateHelper;
    function GetDocumentModel: TdxDocumentModel;
    function GetControl: TWinControl;
    function GetFormatter: TdxBackgroundFormatter;
    function GetIsUpdateLocked: Boolean;
    function GetLayoutCalculationMode: TdxCalculationModeType;
    function GetDocumentLayout: TdxDocumentLayout;
    function GetDocumentLayoutAsync: TdxDocumentLayout;
    procedure PerformPageSecondaryFormatting(APage: TdxPage);
    procedure AddDocumentLayoutInvalidated(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure AddPageFormatted(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure AddDocumentFormatted(const AHandler: TdxEvent);
    procedure RemoveDocumentLayoutInvalidated(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure RemovePageFormatted(const AHandler: TdxDocumentLayoutInvalidatedEvent);
    procedure RemoveDocumentFormatted(const AHandler: TdxEvent);
    // IdxRichEditDocumentServer
    function GetDpiX: Single;
    function GetDpiY: Single;
    function GetModified: Boolean;
    function GetText: string;
    function GetRtfText: string;
    function GetHtmlText: string;
    function GetDocument: IdxRichEditDocument;
    function GetMeasurementUnit: TdxMeasurementUnit;
    function GetLayoutUnit: TdxDocumentLayoutUnit;
    procedure SetModified(Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetRtfText(const AValue: string);
    procedure SetHtmlText(const AValue: string);
    procedure SetMeasurementUnit(Value: TdxMeasurementUnit);
    procedure SetLayoutUnit(Value: TdxDocumentLayoutUnit);
    procedure SetLayoutCalculationMode(const Value: TdxCalculationModeType);
    procedure AddAfterExportHandler(const AHandler: TNotifyEvent);
    procedure RemoveAfterExportHandler(const AHandler: TNotifyEvent);
    procedure AddSelectionChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveSelectionChangedHandler(const AHandler: TNotifyEvent);
    procedure AddDocumentLoadedHandler(const AHandler: TNotifyEvent);
    procedure RemoveDocumentLoadedHandler(const AHandler: TNotifyEvent);
    procedure AddEmptyDocumentCreatedHandler(const AHandler: TNotifyEvent);
    procedure RemoveEmptyDocumentCreatedHandler(const AHandler: TNotifyEvent);
    procedure AddDocumentClosingHandler(const AHandler: TCloseQueryEvent);
    procedure RemoveDocumentClosingHandler(const AHandler: TCloseQueryEvent);
    procedure AddContentChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveContentChangedHandler(const AHandler: TNotifyEvent);
    procedure AddModifiedChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveModifiedChangedHandler(const AHandler: TNotifyEvent);
    procedure AddUnitChangingHandler(const AHandler: TNotifyEvent);
    procedure RemoveUnitChangingHandler(const AHandler: TNotifyEvent);
    procedure AddUnitChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveUnitChangedHandler(const AHandler: TNotifyEvent);
    procedure AddCalculateDocumentVariableHandler(const AHandler: TdxCalculateDocumentVariableEvent);
    procedure RemoveCalculateDocumentVariableHandler(const AHandler: TdxCalculateDocumentVariableEvent);
    procedure AddBeforeImportHandler(const AHandler: TdxBeforeImportEvent);
    procedure RemoveBeforeImportHandler(const AHandler: TdxBeforeImportEvent);
    procedure AddBeforeExportHandler(const AHandler: TdxBeforeExportEvent);
    procedure RemoveBeforeExportHandler(const AHandler: TdxBeforeExportEvent);
    procedure AddInitializeDocumentHandler(const AHandler: TdxEvent);
    procedure RemoveInitializeDocumentHandler(const AHandler: TdxEvent);
    procedure AddRtfTextChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveRtfTextChangedHandler(const AHandler: TNotifyEvent);
    procedure AddHtmlTextChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveHtmlTextChangedHandler(const AHandler: TNotifyEvent);
  strict private
    function GetOptions: TdxRichEditControlOptionsBase;
  protected
    function CreateDocumentContainer(ADocumentModel: TObject): IdxRichEditDocumentContainer;
    function CreateDocumentServer(ADocumentModel: TdxDocumentModel): IdxRichEditDocumentContainer;
    function CreateInnerServer(ADocumentModel: TdxDocumentModel): TdxInnerRichEditDocumentServer; virtual;
    procedure BeginInitialize; virtual;
    procedure EndInitialize; virtual;
    function CreateMeasurementAndDrawingStrategy(ADocumentModel: TdxDocumentModel): TdxMeasurementAndDrawingStrategy;
    function CreateOptions(const ADocumentServer: TObject{TdxInnerRichEditDocumentServer}): TObject{TdxRichEditControlOptionsBase};
    function CreateOptionsCore(ADocumentServer: TdxInnerRichEditDocumentServer): TdxRichEditControlOptionsBase; virtual;
    procedure RaiseDeferredEvents(AChangeActions: TdxDocumentModelChangeActions);

    property BatchUpdateHelper: TdxBatchUpdateHelper read GetBatchUpdateHelper;
    property Formatter: TdxBackgroundFormatter read GetFormatter;
  public
    constructor Create(ADocumentModel: TdxDocumentModel = nil);
    destructor Destroy; override;

    class function TryConvertInternalRichEditDocumentServer(const AObj: TValue): TdxInternalRichEditDocumentServer; static;

    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;
    //IdxServiceContainer
    procedure AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface); overload;
    procedure AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface; APromote: Boolean); overload;
    procedure AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback); overload;
    procedure AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback; APromote: Boolean); overload;
    procedure RemoveService(const AServiceType: TdxServiceType); overload;
    procedure RemoveService(const AServiceType: TdxServiceType; APromote: Boolean); overload;
    //IdxServiceProvider
    function GetService(const AServiceType: TdxServiceType): IInterface; overload;

    function GetService<T: IInterface>: T; overload;

    function CreateNewDocument(ARaiseDocumentClosing: Boolean = False): Boolean;
    procedure LoadDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat); overload; virtual;
    procedure LoadDocument(const AFileName: string); overload; virtual;
    procedure LoadDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat); overload; virtual;
    procedure LoadDocumentTemplate(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat); overload; virtual;
    procedure LoadDocumentTemplate(const AFileName: string); overload; virtual;
    procedure LoadDocumentTemplate(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat); overload; virtual;
    procedure SaveDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat); overload; virtual;
    procedure SaveDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat); overload; virtual;

    function CreateMailMergeOptions: IdxRichEditMailMergeOptions;
    procedure MailMerge(const ADocument: IdxRichEditDocument); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; const ATargetDocument: IdxRichEditDocument); overload;
    procedure MailMerge(const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(AStream: TStream; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; const AFileName: string; AFormat: TdxRichEditDocumentFormat); overload;
    procedure MailMerge(const AOptions: IdxRichEditMailMergeOptions; AStream: TStream; AFormat: TdxRichEditDocumentFormat); overload;

    property Document: IdxRichEditDocument read GetDocument;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property InnerServer: TdxInnerRichEditDocumentServer read FInnerServer;
    property Options: TdxRichEditControlOptionsBase read GetOptions;
    property LayoutCalculationMode: TdxCalculationModeType read GetLayoutCalculationMode write SetLayoutCalculationMode;

    property HtmlText: string read GetHtmlText write SetHtmlText;
  end;

implementation

uses
  dxRichEdit.InnerControl.DrawingStrategy;

{ TdxInternalRichEditDocumentServer }

constructor TdxInternalRichEditDocumentServer.Create(ADocumentModel: TdxDocumentModel = nil);
begin
  inherited Create;
  FInnerServer := CreateInnerServer(ADocumentModel);
  BeginInitialize;
  EndInitialize;
end;

destructor TdxInternalRichEditDocumentServer.Destroy;
begin
  FreeAndNil(FInnerServer);
  inherited Destroy;
end;

class function TdxInternalRichEditDocumentServer.TryConvertInternalRichEditDocumentServer(const AObj: TValue): TdxInternalRichEditDocumentServer;
var
  AOwner: IdxInternalRichEditDocumentServerOwner;
  AObject: TObject;
begin
  Result := nil;
  if not (AObj.IsObject or AObj.IsInterface) then
    Exit;

  if AObj.IsObject then
    AObject := AObj.AsObject
  else
    AObject := TObject(AObj.AsInterface);

  Result := Safe<TdxInternalRichEditDocumentServer>.Cast(AObject);
  if Result <> nil then
    Exit;
  if Supports(AObj.AsObject, IdxInternalRichEditDocumentServerOwner, AOwner) then
    Result := AOwner.InternalServer;
end;

function TdxInternalRichEditDocumentServer.GetLayoutCalculationMode: TdxCalculationModeType;
begin
  Result := InnerServer.LayoutCalculationMode;
end;

function TdxInternalRichEditDocumentServer.GetDocumentLayout: TdxDocumentLayout;
begin
  Result := FInnerServer.CalculatePrintDocumentLayout(Self);
end;

function TdxInternalRichEditDocumentServer.GetDocumentLayoutAsync: TdxDocumentLayout;
begin
  Result := FInnerServer.ModelDocumentLayout;
end;

procedure TdxInternalRichEditDocumentServer.PerformPageSecondaryFormatting(APage: TdxPage);
begin
  Formatter.PerformPageSecondaryFormatting(APage);
end;

procedure TdxInternalRichEditDocumentServer.AddDocumentLayoutInvalidated(
  const AHandler: TdxDocumentLayoutInvalidatedEvent);
begin
  FInnerServer.DocumentLayoutInvalidated.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddPageFormatted(const AHandler: TdxDocumentLayoutInvalidatedEvent);
begin
  FInnerServer.PageFormatted.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddDocumentFormatted(const AHandler: TdxEvent);
begin
  FInnerServer.DocumentFormatted.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveDocumentLayoutInvalidated(
  const AHandler: TdxDocumentLayoutInvalidatedEvent);
begin
  FInnerServer.DocumentLayoutInvalidated.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemovePageFormatted(const AHandler: TdxDocumentLayoutInvalidatedEvent);
begin
  FInnerServer.PageFormatted.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveDocumentFormatted(const AHandler: TdxEvent);
begin
  FInnerServer.DocumentFormatted.Add(AHandler);
end;

function TdxInternalRichEditDocumentServer.GetDpiX: Single;
begin
  Result := InnerServer.DpiX;
end;

function TdxInternalRichEditDocumentServer.GetDpiY: Single;
begin
  Result := InnerServer.DpiY;
end;

function TdxInternalRichEditDocumentServer.GetDocumentModel: TdxDocumentModel;
begin
  if InnerServer <> nil then
    Result := InnerServer.DocumentModel
  else
    Result := nil;
end;

function TdxInternalRichEditDocumentServer.GetControl: TWinControl;
begin
  Result := nil;
end;

function TdxInternalRichEditDocumentServer.GetFormatter: TdxBackgroundFormatter;
begin
  Result := InnerServer.BackgroundFormatter;
end;

function TdxInternalRichEditDocumentServer.GetModified: Boolean;
begin
  Result := (InnerServer <> nil) and InnerServer.Modified;
end;

procedure TdxInternalRichEditDocumentServer.SetModified(Value: Boolean);
begin
  if InnerServer <> nil then
    InnerServer.Modified := Value;
end;

function TdxInternalRichEditDocumentServer.GetText: string;
begin
  if DocumentModel <> nil then
    Result := DocumentModel.InternalAPI.Text
  else
    Result := '';
end;

procedure TdxInternalRichEditDocumentServer.SetText(const Value: string);
begin
  if DocumentModel <> nil then
    DocumentModel.InternalAPI.Text := Value;
end;

function TdxInternalRichEditDocumentServer.GetRtfText: string;
begin
  if DocumentModel <> nil then
    Result := DocumentModel.InternalAPI.RtfText
  else
    Result := '';
end;

procedure TdxInternalRichEditDocumentServer.SetRtfText(const AValue: string);
begin
  if DocumentModel <> nil then
    DocumentModel.InternalAPI.RtfText := AValue;
end;

function TdxInternalRichEditDocumentServer.GetHtmlText: string;
begin
  if DocumentModel <> nil then
    Result := DocumentModel.InternalAPI.HtmlText
  else
    Result := '';
end;

procedure TdxInternalRichEditDocumentServer.SetHtmlText(const AValue: string);
begin
  if DocumentModel <> nil then
    if AValue = '' then
      DocumentModel.InternalAPI.HtmlText := ''
    else
      DocumentModel.InternalAPI.HtmlText := AValue;
end;


function TdxInternalRichEditDocumentServer.GetDocument: IdxRichEditDocument;
begin
  if InnerServer <> nil then
    Result := InnerServer.NativeDocument
  else
    Result := nil;
end;

function TdxInternalRichEditDocumentServer.GetMeasurementUnit: TdxMeasurementUnit;
begin
  if InnerServer <> nil then
    Result := InnerServer.MeasurementUnit
  else
    Result := TdxMeasurementUnit.Document;
end;

procedure TdxInternalRichEditDocumentServer.SetMeasurementUnit(Value: TdxMeasurementUnit);
begin
  if InnerServer <> nil then
    InnerServer.MeasurementUnit := Value;
end;

function TdxInternalRichEditDocumentServer.GetLayoutUnit: TdxDocumentLayoutUnit;
begin
  if InnerServer <> nil then
    Result := InnerServer.LayoutUnit
  else
    Result := TdxDocumentLayoutUnit(TdxDocumentModel.DefaultLayoutUnit);
end;

procedure TdxInternalRichEditDocumentServer.SetLayoutUnit(Value: TdxDocumentLayoutUnit);
begin
  if InnerServer <> nil then
    InnerServer.LayoutUnit := Value;
end;

procedure TdxInternalRichEditDocumentServer.SetLayoutCalculationMode(const Value: TdxCalculationModeType);
begin
  InnerServer.LayoutCalculationMode := Value;
end;

procedure TdxInternalRichEditDocumentServer.AddAfterExportHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.AfterExport.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveAfterExportHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.AfterExport.Remove(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddSelectionChangedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.SelectionChanged.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveSelectionChangedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.SelectionChanged.Remove(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddDocumentLoadedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.DocumentLoaded.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveDocumentLoadedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.DocumentLoaded.Remove(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddEmptyDocumentCreatedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.EmptyDocumentCreated.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveEmptyDocumentCreatedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.EmptyDocumentCreated.Remove(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddDocumentClosingHandler(const AHandler: TCloseQueryEvent);
begin
  if InnerServer <> nil then
    InnerServer.DocumentClosing.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveDocumentClosingHandler(const AHandler: TCloseQueryEvent);
begin
  if InnerServer <> nil then
    InnerServer.DocumentClosing.Remove(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddContentChangedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.ContentChanged.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveContentChangedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.ContentChanged.Remove(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddModifiedChangedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.ModifiedChanged.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveModifiedChangedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.ModifiedChanged.Remove(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddUnitChangingHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.UnitChanging.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveUnitChangingHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.UnitChanging.Remove(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddUnitChangedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.UnitChanged.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveUnitChangedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.UnitChanged.Remove(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddCalculateDocumentVariableHandler(const AHandler: TdxCalculateDocumentVariableEvent);
begin
  if InnerServer <> nil then
    InnerServer.CalculateDocumentVariable.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveCalculateDocumentVariableHandler(const AHandler: TdxCalculateDocumentVariableEvent);
begin
  if InnerServer <> nil then
    InnerServer.CalculateDocumentVariable.Remove(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddBeforeImportHandler(const AHandler: TdxBeforeImportEvent);
begin
  if InnerServer <> nil then
    InnerServer.BeforeImport.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveBeforeImportHandler(const AHandler: TdxBeforeImportEvent);
begin
  if InnerServer <> nil then
    InnerServer.BeforeImport.Remove(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddBeforeExportHandler(const AHandler: TdxBeforeExportEvent);
begin
  if InnerServer <> nil then
    InnerServer.BeforeExport.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveBeforeExportHandler(const AHandler: TdxBeforeExportEvent);
begin
  if InnerServer <> nil then
    InnerServer.BeforeExport.Remove(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddInitializeDocumentHandler(const AHandler: TdxEvent);
begin
  if InnerServer <> nil then
    InnerServer.InitializeDocument.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveInitializeDocumentHandler(const AHandler: TdxEvent);
begin
  if InnerServer <> nil then
    InnerServer.InitializeDocument.Remove(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddRtfTextChangedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.RtfTextChanged.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveRtfTextChangedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.RtfTextChanged.Remove(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.AddHtmlTextChangedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.HtmlTextChanged.Add(AHandler);
end;

procedure TdxInternalRichEditDocumentServer.RemoveHtmlTextChangedHandler(const AHandler: TNotifyEvent);
begin
  if InnerServer <> nil then
    InnerServer.HtmlTextChanged.Remove(AHandler);
end;

function TdxInternalRichEditDocumentServer.GetOptions: TdxRichEditControlOptionsBase;
begin
  if InnerServer <> nil then
    Result := InnerServer.Options
  else
    Result := nil;
end;

function TdxInternalRichEditDocumentServer.CreateDocumentContainer(ADocumentModel: TObject): IdxRichEditDocumentContainer;
begin
  Result := CreateDocumentServer(TdxDocumentModel(ADocumentModel));
end;

function TdxInternalRichEditDocumentServer.CreateDocumentServer(ADocumentModel: TdxDocumentModel): IdxRichEditDocumentContainer;
begin
  Result := CreateInnerServer(ADocumentModel);
end;

function TdxInternalRichEditDocumentServer.CreateInnerServer(ADocumentModel: TdxDocumentModel): TdxInnerRichEditDocumentServer;
begin
  if ADocumentModel = nil then
    Result := TdxInnerRichEditDocumentServer.Create(Self)
  else
    Result := TdxInnerRichEditDocumentServer.Create(Self, ADocumentModel);
end;

procedure TdxInternalRichEditDocumentServer.BeginInitialize;
begin
  InnerServer.BeginInitialize;
end;

procedure TdxInternalRichEditDocumentServer.EndInitialize;
begin
  InnerServer.EndInitialize;
end;

function TdxInternalRichEditDocumentServer.CreateMeasurementAndDrawingStrategy(ADocumentModel: TdxDocumentModel): TdxMeasurementAndDrawingStrategy;
begin
  Result := TdxServerGdiMeasurementAndDrawingStrategy.Create(ADocumentModel);
end;

function TdxInternalRichEditDocumentServer.CreateOptions(const ADocumentServer: TObject{TdxInnerRichEditDocumentServer}): TObject{TdxRichEditControlOptionsBase};
begin
  Result := CreateOptionsCore(TdxInnerRichEditDocumentServer(ADocumentServer));
end;

function TdxInternalRichEditDocumentServer.CreateOptionsCore(ADocumentServer: TdxInnerRichEditDocumentServer): TdxRichEditControlOptionsBase;
begin
  Result := TdxRichEditDocumentServerOptions.Create(ADocumentServer);
end;

procedure TdxInternalRichEditDocumentServer.RaiseDeferredEvents(AChangeActions: TdxDocumentModelChangeActions);
begin
  InnerServer.RaiseDeferredEventsCore(AChangeActions);
end;

function TdxInternalRichEditDocumentServer.GetBatchUpdateHelper: TdxBatchUpdateHelper;
var
  AUpdateable: IdxBatchUpdateable;
begin
  AUpdateable := InnerServer as IdxBatchUpdateable;
  Result := AUpdateable.BatchUpdateHelper;
end;

procedure TdxInternalRichEditDocumentServer.BeginUpdate;
begin
  if InnerServer <> nil then
    InnerServer.BeginUpdate;
end;

procedure TdxInternalRichEditDocumentServer.CancelUpdate;
begin
  if InnerServer <> nil then
    InnerServer.CancelUpdate;
end;

procedure TdxInternalRichEditDocumentServer.EndUpdate;
begin
  if InnerServer <> nil then
    InnerServer.EndUpdate;
end;

function TdxInternalRichEditDocumentServer.GetIsUpdateLocked: Boolean;
begin
  Result := (InnerServer <> nil) and InnerServer.IsUpdateLocked;
end;

procedure TdxInternalRichEditDocumentServer.AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback; APromote: Boolean);
begin
  if InnerServer <> nil then
    (InnerServer as IdxServiceContainer).AddService(AServiceType, ACallback, APromote);
end;

procedure TdxInternalRichEditDocumentServer.AddService(const AServiceType: TdxServiceType; const ACallback: IdxServiceCreatorCallback);
begin
  if InnerServer <> nil then
    (InnerServer as IdxServiceContainer).AddService(AServiceType, ACallback);
end;

procedure TdxInternalRichEditDocumentServer.AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface; APromote: Boolean);
begin
  if InnerServer <> nil then
    (InnerServer as IdxServiceContainer).AddService(AServiceType, AServiceInstance, APromote);
end;

procedure TdxInternalRichEditDocumentServer.AddService(const AServiceType: TdxServiceType; const AServiceInstance: IInterface);
begin
  if InnerServer <> nil then
    (InnerServer as IdxServiceContainer).AddService(AServiceType, AServiceInstance);
end;

procedure TdxInternalRichEditDocumentServer.RemoveService(const AServiceType: TdxServiceType; APromote: Boolean);
begin
  if InnerServer <> nil then
    (InnerServer as IdxServiceContainer).RemoveService(AServiceType, APromote);
end;

procedure TdxInternalRichEditDocumentServer.RemoveService(const AServiceType: TdxServiceType);
begin
  if InnerServer <> nil then
    (InnerServer as IdxServiceContainer).RemoveService(AServiceType);
end;

function TdxInternalRichEditDocumentServer.GetService(const AServiceType: TdxServiceType): IInterface;
begin
  if InnerServer <> nil then
    Result := (InnerServer as IdxServiceContainer).GetService(AServiceType)
  else
    Result := nil;
end;

function TdxInternalRichEditDocumentServer.GetService<T>: T;
begin
  if InnerServer <> nil then
    Result := InnerServer.GetService<T>
  else
    Result := Default(T);
end;

function TdxInternalRichEditDocumentServer.CreateNewDocument(ARaiseDocumentClosing: Boolean = False): Boolean;
begin
  if InnerServer <> nil then
    Result := InnerServer.CreateNewDocument(ARaiseDocumentClosing)
  else
    Result := True;
end;

procedure TdxInternalRichEditDocumentServer.LoadDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  if InnerServer <> nil then
    InnerServer.LoadDocument(AStream, ADocumentFormat);
end;

procedure TdxInternalRichEditDocumentServer.SaveDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  if InnerServer <> nil then
    InnerServer.SaveDocument(AStream, ADocumentFormat);
end;

procedure TdxInternalRichEditDocumentServer.LoadDocumentTemplate(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  if InnerServer <> nil then
    InnerServer.LoadDocumentTemplate(AStream, ADocumentFormat);
end;

procedure TdxInternalRichEditDocumentServer.LoadDocument(const AFileName: string);
begin
  if InnerServer <> nil then
    InnerServer.LoadDocument(AFileName);
end;

procedure TdxInternalRichEditDocumentServer.LoadDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  if InnerServer <> nil then
    InnerServer.LoadDocument(AFileName, ADocumentFormat);
end;

procedure TdxInternalRichEditDocumentServer.LoadDocumentTemplate(const AFileName: string);
begin
  if InnerServer <> nil then
    InnerServer.LoadDocumentTemplate(AFileName);
end;

procedure TdxInternalRichEditDocumentServer.LoadDocumentTemplate(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  if InnerServer <> nil then
    InnerServer.LoadDocumentTemplate(AFileName, ADocumentFormat);
end;

procedure TdxInternalRichEditDocumentServer.SaveDocument(const AFileName: string; ADocumentFormat: TdxRichEditDocumentFormat);
begin
  if InnerServer <> nil then
    InnerServer.SaveDocument(AFileName, ADocumentFormat);
end;

function TdxInternalRichEditDocumentServer.CreateMailMergeOptions: IdxRichEditMailMergeOptions;
begin
  if InnerServer <> nil then
    Result := InnerServer.CreateMailMergeOptions
  else
  begin
    Result := nil;
  end;
end;

procedure TdxInternalRichEditDocumentServer.MailMerge(const ADocument: IdxRichEditDocument);
begin
  if InnerServer <> nil then
    InnerServer.MailMerge(ADocument);
end;

procedure TdxInternalRichEditDocumentServer.MailMerge(const AOptions: IdxRichEditMailMergeOptions; const ATargetDocument: IdxRichEditDocument);
begin
  if InnerServer <> nil then
    InnerServer.MailMerge(AOptions, ATargetDocument);
end;

procedure TdxInternalRichEditDocumentServer.MailMerge(const AFileName: string; AFormat: TdxRichEditDocumentFormat);
begin
  if InnerServer <> nil then
    InnerServer.MailMerge(AFileName, AFormat);
end;

procedure TdxInternalRichEditDocumentServer.MailMerge(AStream: TStream; AFormat: TdxRichEditDocumentFormat);
begin
  if InnerServer <> nil then
    InnerServer.MailMerge(AStream, AFormat);
end;

procedure TdxInternalRichEditDocumentServer.MailMerge(const AOptions: IdxRichEditMailMergeOptions; const AFileName: string; AFormat: TdxRichEditDocumentFormat);
begin
  if InnerServer <> nil then
    InnerServer.MailMerge(AOptions, AFileName, AFormat);
end;

procedure TdxInternalRichEditDocumentServer.MailMerge(const AOptions: IdxRichEditMailMergeOptions; AStream: TStream; AFormat: TdxRichEditDocumentFormat);
begin
  if InnerServer <> nil then
    InnerServer.MailMerge(AOptions, AStream, AFormat);
end;

end.
