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

unit dxRichEdit.Export.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Controls, Dialogs, Contnrs,
  Generics.Defaults, Generics.Collections,
  dxCoreClasses,

  dxRichEdit.NativeApi,
  dxEncoding,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Utils.FileDialogFilter,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.Utils.ProgressIndication,
  dxRichEdit.ImportExportHelper;

type

  { IdxExporterOptions }

  IdxExporterOptions = interface(IdxSupportsCopyFrom<TObject{IdxExporterOptions}>)
  ['{D8B1FA81-27F7-41B5-88DA-79768D4934BE}']
    function GetActualEncoding: TEncoding;
    function GetTargetUri: string;
    procedure SetActualEncoding(const Value: TEncoding);
    procedure SetTargetUri(const Value: string);

    property ActualEncoding: TEncoding read GetActualEncoding write SetActualEncoding;
    property TargetUri: string read GetTargetUri write SetTargetUri;
  end;

  { IdxExporter }

  IdxExporter = interface
  ['{15211C02-8DBE-4310-929D-52CE05B945B4}']
    function Filter: TdxFileDialogFilter;
    function Format: TdxRichEditDocumentFormat;
    function FormatEquals(const AValue: TdxRichEditDocumentFormat): Boolean;
    function SupportsEncryption: Boolean;
    function SetupSaving: TObject;
    function SaveDocument(const ADocumentModel: TdxCustomDocumentModel; AStream: TStream;
      const AOptions: IdxExporterOptions): Boolean;
  end;

  { TdxExporterList }

  TdxExporterList = class(TInterfaceList)
  private
    function GetItem(Index: Integer): IdxExporter;
  public
    function First: IdxExporter; reintroduce;
    function Last: IdxExporter; reintroduce;

    property Items[Index: Integer]: IdxExporter read GetItem; default;
  end;

  { IdxExportManagerService }

  IdxExportManagerService = interface
  ['{8F747760-6D4E-4206-9F55-5775DA0BBF9C}']
    procedure RegisterExporter(const AExporter: IdxExporter);
    procedure UnregisterExporter(const AExporter: IdxExporter);
    procedure UnregisterAllExporters;
    function GetExporter(AFormat: TdxRichEditDocumentFormat): IdxExporter;
    function GetExporters: TdxExporterList;
  end;

  TdxExportersCalculator = function(const AExportManagerService: IdxExportManagerService): TdxExporterList;

  IdxDocumentExportManagerService = interface(IdxExportManagerService)
  ['{D2FC9E28-92AE-4C08-A34A-136287D60B46}']
  end;

  { TdxBeforeExportEventArgs }

  TdxBeforeExportEventArgs = class(TdxEventArgs)
  private
    FFormat: TdxRichEditDocumentFormat;
    FOptions: IdxExporterOptions;
  public
    constructor Create(AFormat: TdxRichEditDocumentFormat; const AOptions: IdxExporterOptions);
    property Format: TdxRichEditDocumentFormat read FFormat;
    property Options: IdxExporterOptions read FOptions;
  end;

  TdxBeforeExportEvent = procedure(Sender: TObject; E: TdxBeforeExportEventArgs) of object;
  TdxBeforeExportEventHandler = TdxMulticastMethod<TdxBeforeExportEvent>;

  { TdxSaveFileDialog }

  TdxSaveFileDialog = class(TSaveDialog)
  private
    function GetCheckFileExists: Boolean;
    function GetCheckPathExists: Boolean;
    function GetDereferenceLinks: Boolean;
    function GetOverwritePrompt: Boolean;
    function GetRestoreDirectory: Boolean;
    function GetValidateNames: Boolean;
    procedure SetCheckFileExists(const Value: Boolean);
    procedure SetCheckPathExists(const Value: Boolean);
    procedure SetDereferenceLinks(const Value: Boolean);
    procedure SetOverwritePrompt(const Value: Boolean);
    procedure SetRestoreDirectory(const Value: Boolean);
    procedure SetValidateNames(const Value: Boolean);
  public
    property CheckFileExists: Boolean read GetCheckFileExists write SetCheckFileExists;
    property CheckPathExists: Boolean read GetCheckPathExists write SetCheckPathExists;
    property DereferenceLinks: Boolean read GetDereferenceLinks write SetDereferenceLinks;
    property OverwritePrompt: Boolean read GetOverwritePrompt write SetOverwritePrompt;
    property RestoreDirectory: Boolean read GetRestoreDirectory write SetRestoreDirectory;
    property ValidateNames: Boolean read GetValidateNames write SetValidateNames;
  end;

  { TdxExportManagerService }

  TdxExportManagerService = class abstract(TInterfacedObject,
    IdxExportManagerService)
  private
    FExporters: TDictionary<TdxRichEditDocumentFormat, IdxExporter>;
  protected
    procedure RegisterNativeFormats; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    //IdxExportManagerService<TFormat, TResult>
    procedure RegisterExporter(const AExporter: IdxExporter); virtual;
    procedure UnregisterExporter(const AExporter: IdxExporter); virtual;
    procedure UnregisterAllExporters;
    function GetExporter(AFormat: TdxRichEditDocumentFormat): IdxExporter; virtual;
    function GetExporters: TdxExporterList; virtual;

    property Exporters: TDictionary<TdxRichEditDocumentFormat, IdxExporter> read FExporters;
  end;

  { TdxDocumentExportManagerService }

  TdxDocumentExportManagerService = class(TdxExportManagerService, IdxDocumentExportManagerService)
  protected
    procedure RegisterNativeFormats; override;
  end;

  { TdxExportTarget }

  TdxExportTarget = class
  private
    FFileName: string;
    FExporter: IdxExporter;
  public
    constructor Create(const AFileName: string; const AExporter: IdxExporter);
    function GetStream: TStream; virtual;

    property FileName: string read FFileName;
    property Exporter: IdxExporter read FExporter;
    property Storage: string read FFileName;
  end;

  { TdxExportHelper<TFormat, TResult> }

  TdxExportHelper = class abstract(TdxImportExportHelper)
  protected
    procedure ApplyEncoding(const AOptions: IdxExporterOptions; AEncoding: TEncoding); virtual; abstract;
    function CalculateCurrentFilterIndex(AExporters: TdxExporterList): Integer; virtual;
    function ChooseExporter(const AFileName: string; AFilterIndex: Integer; AExporters: TdxExporterList): IdxExporter;
    function ChooseExporterByFileName(const AFileName: string; AExporters: TdxExporterList): IdxExporter;
    function ChooseExporterByFilterIndex(AFilterIndex: Integer; AExporters: TdxExporterList): IdxExporter;
    function CreateExportFilters(AExporters: TdxExporterList): TdxFileDialogFilterCollection; virtual;
    function CreateSaveFileDialog(AFilters: TdxFileDialogFilterCollection; ACurrentFilterIndex: Integer): TdxSaveFileDialog; virtual;
    function GetCurrentDocumentFormat: TdxRichEditDocumentFormat; virtual; abstract;
    function GetDirectoryName(const AFileName: string): string; virtual;
    function GetFileName(ADialog: TdxSaveFileDialog): string; overload;
    function GetFileName(const AFileName: string): string; overload; virtual;
    function GetFileNameForSaving: string; virtual; abstract;
    function GetFileStorage(ADialog: TdxSaveFileDialog): string;
    function GetPredefinedOptions(AFormat: TdxRichEditDocumentFormat): IdxExporterOptions; virtual; abstract;
    procedure PreprocessContentBeforeExport(AFormat: TdxRichEditDocumentFormat); virtual; abstract;
    procedure SetDirectoryName(ADialog: TdxSaveFileDialog; const ADirectoryName: string);
    procedure SetFileName(ADialog: TdxSaveFileDialog; const AFileName: string);
    function ShowSaveFileDialog(ADialog: TdxSaveFileDialog; const AParent: TWinControl): Boolean; virtual;
  public
    function Export(AStream: TStream; AFormat: TdxRichEditDocumentFormat; const ATargetUri: string; const AExportManagerService: IdxExportManagerService): Boolean; overload;
    function Export(AStream: TStream; AFormat: TdxRichEditDocumentFormat; const ATargetUri: string; const AExportManagerService: IdxExportManagerService; AEncoding: TEncoding): Boolean; overload;
    function InvokeExportDialog(const AParent: TWinControl; const AExportManagerService: IdxExportManagerService): TdxExportTarget; overload;
    function InvokeExportDialog(const AParent: TWinControl; const AExportManagerService: IdxExportManagerService;
      AExportersCollector: TdxExportersCalculator): TdxExportTarget; overload;
  end;

  { TdxCustomDocumentModelExporter }

  TdxCustomDocumentModelExporter = class abstract(TInterfacedObject)
  private
    FDocumentModel: TdxCustomDocumentModel;
    FLastParagraphRunNotSelected: Boolean;
    FKeepFieldCodeViewState: Boolean;
    FOptions: IdxExporterOptions;
    FProgressIndication: TdxProgressIndication;
    function GetUnitConverter: TdxDocumentModelUnitConverter;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxExporterOptions); virtual;
    destructor Destroy; override;

    function ExportSaveMemory: TdxChunkedStringBuilder; virtual;
    procedure Export(AOutputStream: TStream); overload; virtual; abstract;

    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
    property KeepFieldCodeViewState: Boolean read FKeepFieldCodeViewState write FKeepFieldCodeViewState;
    property LastParagraphRunNotSelected: Boolean read FLastParagraphRunNotSelected write FLastParagraphRunNotSelected;
    property Options: IdxExporterOptions read FOptions;
    property ProgressIndication: TdxProgressIndication read FProgressIndication;
    property UnitConverter: TdxDocumentModelUnitConverter read  GetUnitConverter;
  end;

implementation

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  IOUtils, Forms, Windows,
  dxCore,
  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.Strs,
  dxRichEdit.Export.Formats;

{ TdxExporterList }

function TdxExporterList.GetItem(Index: Integer): IdxExporter;
begin
  Result := inherited Items[Index] as IdxExporter;
end;

function TdxExporterList.First: IdxExporter;
begin
  Result := inherited First as IdxExporter;
end;

function TdxExporterList.Last: IdxExporter;
begin
  Result := inherited Last as IdxExporter;
end;

{ TdxBeforeExportEventArgs }

constructor TdxBeforeExportEventArgs.Create(AFormat: TdxRichEditDocumentFormat; const AOptions: IdxExporterOptions);
begin
  FFormat := AFormat;
  FOptions := AOptions;
end;

{ TdxExportManagerService<TFormat, TResult> }

constructor TdxExportManagerService.Create;
begin
  inherited Create;
  FExporters := TObjectDictionary<TdxRichEditDocumentFormat, IdxExporter>.Create;
  RegisterNativeFormats;
end;

destructor TdxExportManagerService.Destroy;
begin
  FreeAndNil(FExporters);
  inherited Destroy;
end;

procedure TdxExportManagerService.RegisterExporter(const AExporter: IdxExporter);
begin
  Exporters.Add(AExporter.Format, AExporter);
end;

procedure TdxExportManagerService.UnregisterExporter(const AExporter: IdxExporter);
begin
  if AExporter <> nil then
    Exporters.Remove(AExporter.Format);
end;

procedure TdxExportManagerService.UnregisterAllExporters;
begin
  Exporters.Clear;
end;

function TdxExportManagerService.GetExporter(AFormat: TdxRichEditDocumentFormat): IdxExporter;
begin
  if not Exporters.TryGetValue(AFormat, Result) then
    Result := nil;
end;

function TdxExportManagerService.GetExporters: TdxExporterList;
var
  AExporter: IdxExporter;
begin
  Result := TdxExporterList.Create;
  for AExporter in Exporters.Values do
    Result.Add(AExporter);
end;

{ TdxDocumentExportManagerService }

procedure TdxDocumentExportManagerService.RegisterNativeFormats;
begin
  TdxExportFileFormats.RegisterDocumentExportFormats(Self);
end;

{ TdxExportHelper<TFormat, TResult> }

function TdxExportHelper.Export(AStream: TStream; AFormat: TdxRichEditDocumentFormat; const ATargetUri: string;
  const AExportManagerService: IdxExportManagerService): Boolean;
begin
  Result := Export(AStream, AFormat, ATargetUri, AExportManagerService, nil);
end;

function TdxExportHelper.Export(AStream: TStream; AFormat: TdxRichEditDocumentFormat; const ATargetUri: string;
  const AExportManagerService: IdxExportManagerService; AEncoding: TEncoding): Boolean;
var
  AExporter: IdxExporter;
  AOptions, APredefinedOptions: IdxExporterOptions;
  AOptionsObject: TObject;
  AEncryptionPropertiesContainer: IdxDocumentEncryptionPropertiesContainer;
begin
  AExporter := AExportManagerService.GetExporter(AFormat);
  if AExporter = nil then
    ThrowUnsupportedFormatException;
  AEncryptionPropertiesContainer := DocumentModel as IdxDocumentEncryptionPropertiesContainer;
  if (AEncryptionPropertiesContainer <> nil) and (AEncryptionPropertiesContainer.EncryptionProperties.Password <> '') and not AExporter.SupportsEncryption then
    if Application.MessageBox(PChar(cxGetResourceString(@sdxRichEditConfirmSaveDocumentWithPasswordProtection)),
      PChar(Application.Title), MB_YESNO + MB_ICONWARNING) <> ID_YES then
      Abort;

  AOptionsObject := AExporter.SetupSaving;
  try
    Supports(AOptionsObject, IdxExporterOptions, AOptions);
    APredefinedOptions := GetPredefinedOptions(AFormat);
    if APredefinedOptions <> nil then
      AOptions.CopyFrom(TObject(APredefinedOptions));
    AOptions.TargetUri := ATargetUri;
    if AEncoding <> nil then
      ApplyEncoding(AOptions, AEncoding);
    PreprocessContentBeforeExport(AFormat);
    Result := AExporter.SaveDocument(DocumentModel, AStream, AOptions);
  finally
    AOptions := nil;
    AOptionsObject.Free;
  end;
end;

function TdxExportHelper.InvokeExportDialog(const AParent: TWinControl;
  const AExportManagerService: IdxExportManagerService): TdxExportTarget;
begin
  Result := InvokeExportDialog(AParent, AExportManagerService, nil);
end;

function TdxExportHelper.InvokeExportDialog(const AParent: TWinControl;
  const AExportManagerService: IdxExportManagerService;
  AExportersCollector: TdxExportersCalculator): TdxExportTarget;
var
  AExporters: TdxExporterList;
  AFilters: TdxFileDialogFilterCollection;
  ACurrentFilterIndex: Integer;
  ADialog: TdxSaveFileDialog;
  AExporter: IdxExporter;
begin
  if AExportManagerService = nil then
    ThrowUnsupportedFormatException;

  if Assigned(AExportersCollector) then
    AExporters := AExportersCollector(AExportManagerService)
  else
    AExporters := AExportManagerService.GetExporters;
  try
    if AExporters.Count <= 0 then
      ThrowUnsupportedFormatException;
    AFilters := CreateExportFilters(AExporters);
    try
      ACurrentFilterIndex := CalculateCurrentFilterIndex(AExporters);
      ADialog := CreateSaveFileDialog(AFilters, ACurrentFilterIndex);
      try
        if not ShowSaveFileDialog(ADialog, AParent) then
          Exit(nil);
        AParent.Repaint;
        AExporter := ChooseExporter(GetFileName(ADialog), ADialog.FilterIndex - 1, AExporters);
        if AExporter = nil then
          ThrowUnsupportedFormatException;
        Result := TdxExportTarget.Create(GetFileStorage(ADialog), AExporter);
      finally
        ADialog.Free;
      end;
    finally
      AFilters.Free;
    end;
  finally
    if not Assigned(AExportersCollector) then
      AExporters.Free;
  end;
end;

function TdxExportHelper.ShowSaveFileDialog(ADialog: TdxSaveFileDialog;
  const AParent: TWinControl): Boolean;
begin
  Result := ADialog.Execute;
end;

function TdxExportHelper.CreateSaveFileDialog(AFilters: TdxFileDialogFilterCollection; ACurrentFilterIndex: Integer): TdxSaveFileDialog;
var
  AFileName, ADirectoryName: string;
begin
  Result := TdxSaveFileDialog.Create(nil);
  Result.Filter := CreateFilterString(AFilters);
  Result.RestoreDirectory := True;
  Result.CheckFileExists := False;
  Result.CheckPathExists := True;
  Result.OverwritePrompt := True;
  Result.DereferenceLinks := True;
  Result.ValidateNames := True;
  Result.FilterIndex := 1;
  if (AFilters.Count > 0) and (ACurrentFilterIndex < AFilters.Count) then
  begin
    Result.FilterIndex := 1 + ACurrentFilterIndex;
    if AFilters[ACurrentFilterIndex].Extensions.Count > 0 then
    begin
      Result.DefaultExt := AFilters[ACurrentFilterIndex].Extensions[0];
    end;
  end;

  AFileName := GetFileNameForSaving;
  ADirectoryName := GetDirectoryName(AFileName);
  AFileName := GetFileName(AFileName);
  SetFileName(Result, AFileName);
  SetDirectoryName(Result, ADirectoryName);
end;

function TdxExportHelper.GetFileName(const AFileName: string): string;
begin
  Result := TPath.GetFileNameWithoutExtension(AFileName);
end;

function TdxExportHelper.GetDirectoryName(const AFileName: string): string;
begin
  if AFileName = '' then
    Result := ''
  else
  begin
    Result := TPath.GetDirectoryName(AFileName);
    if Result = '' then
      Exit;
    Result := TPath.GetFullPath(Result);
  end;
end;

function TdxExportHelper.ChooseExporter(const AFileName: string;
  AFilterIndex: Integer; AExporters: TdxExporterList): IdxExporter;
begin
  Result := ChooseExporterByFileName(AFileName, AExporters);
  if Result = nil then
    Result := ChooseExporterByFilterIndex(AFilterIndex, AExporters);
end;

function TdxExportHelper.ChooseExporterByFileName(const AFileName: string;
  AExporters: TdxExporterList): IdxExporter;
var
  AExtension: string;
  ACount, I: Integer;
  AExtensions: TStrings;
begin
  Result := nil;
  AExtension := LowerCase(TPath.GetExtension(AFileName));
  if AExtension = '' then
    Exit;
  if AExtension[1] = '.' then
    Delete(AExtension, 1, 1);

  ACount := AExporters.Count;
  for I := 0 to ACount - 1 do
  begin
    AExtensions := AExporters[I].Filter.Extensions;
    if AExtensions.IndexOf(AExtension) >= 0 then
    begin
      Result := AExporters[I];
      Break;
    end;
  end;
end;

function TdxExportHelper.ChooseExporterByFilterIndex(AFilterIndex: Integer; AExporters: TdxExporterList): IdxExporter;
begin
  if (AFilterIndex >= 0) and (AFilterIndex < AExporters.Count) then
    Result := AExporters[AFilterIndex]
  else
    Result := nil;
end;

function TdxExportHelper.CreateExportFilters(AExporters: TdxExporterList): TdxFileDialogFilterCollection;
var
  I: Integer;
  AFilter: TdxFileDialogFilter;
  ACount: Integer;
begin
  Result := TdxFileDialogFilterCollection.Create;
  ACount := AExporters.Count;
  for I := 0 to ACount - 1 do
  begin
    AFilter := AExporters[I].Filter;
    if AFilter.Extensions.Count > 0 then
      Result.Add(AFilter.Clone);
  end;
end;

function TdxExportHelper.CalculateCurrentFilterIndex(AExporters: TdxExporterList): Integer;
var
  ADocumentFormat: TdxRichEditDocumentFormat;
  I: Integer;
begin
  ADocumentFormat := GetCurrentDocumentFormat;
  for I := 0 to AExporters.Count - 1 do
    if AExporters[I].FormatEquals(ADocumentFormat) then
      Exit(I);
  Result := 0;
end;

function TdxExportHelper.GetFileStorage(ADialog: TdxSaveFileDialog): string;
begin
  Result := ADialog.FileName;
end;

function TdxExportHelper.GetFileName(ADialog: TdxSaveFileDialog): string;
begin
  Result := ADialog.FileName;
end;

procedure TdxExportHelper.SetFileName(ADialog: TdxSaveFileDialog; const AFileName: string);
begin
  ADialog.FileName := AFileName;
end;

procedure TdxExportHelper.SetDirectoryName(ADialog: TdxSaveFileDialog; const ADirectoryName: string);
begin
  ADialog.InitialDir := ADirectoryName;
end;

{ TdxExportTarget }

constructor TdxExportTarget.Create(const AFileName: string; const AExporter: IdxExporter);
begin
  inherited Create;
  FExporter := AExporter;
  FFileName := AFileName;
end;

function TdxExportTarget.GetStream: TStream;
begin
  Result := TFileStream.Create(FileName, fmOpenReadWrite);
end;

{ TdxSaveFileDialog }

function TdxSaveFileDialog.GetCheckFileExists: Boolean;
begin
  Result := ofFileMustExist in Options;
end;

function TdxSaveFileDialog.GetCheckPathExists: Boolean;
begin
  Result := ofPathMustExist in Options;
end;

function TdxSaveFileDialog.GetDereferenceLinks: Boolean;
begin
  Result := not (ofNoDereferenceLinks in Options);
end;

function TdxSaveFileDialog.GetOverwritePrompt: Boolean;
begin
  Result := ofOverwritePrompt in Options;
end;

function TdxSaveFileDialog.GetRestoreDirectory: Boolean;
begin
  Result := False;
end;

function TdxSaveFileDialog.GetValidateNames: Boolean;
begin
  Result := not (ofNoValidate in Options);
end;

procedure TdxSaveFileDialog.SetCheckFileExists(const Value: Boolean);
begin
  if Value then
    Options := Options + [ofFileMustExist]
  else
    Options := Options - [ofFileMustExist];
end;

procedure TdxSaveFileDialog.SetCheckPathExists(const Value: Boolean);
begin
  if Value then
    Options := Options + [ofPathMustExist]
  else
    Options := Options - [ofPathMustExist];
end;

procedure TdxSaveFileDialog.SetDereferenceLinks(const Value: Boolean);
begin
  if Value then
    Options := Options - [ofNoDereferenceLinks]
  else
    Options := Options + [ofNoDereferenceLinks];
end;

procedure TdxSaveFileDialog.SetOverwritePrompt(const Value: Boolean);
begin
  if Value then
    Options := Options + [ofOverwritePrompt]
  else
    Options := Options - [ofOverwritePrompt];
end;

procedure TdxSaveFileDialog.SetRestoreDirectory(const Value: Boolean);
begin
end;

procedure TdxSaveFileDialog.SetValidateNames(const Value: Boolean);
begin
  if Value then
    Options := Options - [ofNoValidate]
  else
    Options := Options + [ofNoValidate];
end;

{ TdxCustomDocumentModelExporter }

constructor TdxCustomDocumentModelExporter.Create(
  ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxExporterOptions);
begin
  if ADocumentModel = nil then
    raise EArgumentNilException.Create('ADocumentModel');
  inherited Create;
  FDocumentModel := ADocumentModel;
  FOptions := AOptions;
  FProgressIndication := TdxProgressIndication.Create(ADocumentModel);
end;

destructor TdxCustomDocumentModelExporter.Destroy;
begin
  FProgressIndication.Free;
  FOptions := nil;
  inherited Destroy;
end;

function TdxCustomDocumentModelExporter.ExportSaveMemory: TdxChunkedStringBuilder;
begin
  raise ENotSupportedException.CreateFmt('%s doesn''t implement ExportSaveMemory', [ClassName]);
end;

function TdxCustomDocumentModelExporter.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := DocumentModel.UnitConverter;
end;

end.
