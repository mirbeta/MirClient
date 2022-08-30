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

unit dxRichEdit.Import.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Controls, Contnrs, Generics.Defaults, Generics.Collections, Dialogs,
  dxCoreClasses,

  dxRichEdit.NativeApi,
  dxRichEdit.Options.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.ImportExportHelper,
  dxEncoding,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.ProgressIndication,
  dxRichEdit.Utils.FileDialogFilter;

type
  IdxImporterOptions = interface(IdxSupportsCopyFrom<TObject>)
  ['{D8B1FA81-27F7-41B5-88DA-79768D4934BE}']
    function GetSourceUri: string;
    procedure SetSourceUri(const Value: string);

    property SourceUri: string read GetSourceUri write SetSourceUri;
  end;

  IdxCustomImporter = interface
  ['{A7CCAA40-ECB6-4458-90A0-177BDA968607}']
    function Filter: TdxFileDialogFilter;
    function SetupLoading: TObject{IdxImporterOptions};
  end;

  IdxImporter<TFormat, TResult> = interface(IdxCustomImporter)
    function Format: TFormat;
    function LoadDocument(const ADocumentModel: TdxCustomDocumentModel; AStream: TStream;
      const AOptions: IdxImporterOptions): TResult;
  end;

  IdxImportManagerService<TFormat, TResult> = interface
  ['{E445F984-12E2-486C-A60A-3E02F0F8C875}']
    function GetImporter(const AFormat: TFormat): IdxImporter<TFormat, TResult>;
    function GetImporters: TList<IdxCustomImporter>;
    procedure RegisterImporter(const AImporter: IdxImporter<TFormat, TResult>);
    procedure UnregisterAllImporters;
    procedure UnregisterImporter(const AImporter: IdxImporter<TFormat, TResult> );
  end;

  IdxDocumentImportManagerService = interface(IdxImportManagerService<TdxRichEditDocumentFormat, Boolean>)
  ['{A27D6375-2D98-4775-97AF-BC6B99B33712}']
  end;

  TdxBeforeImportEventArgs = class(TdxEventArgs)
  strict private
    FDocumentFormat: TdxRichEditDocumentFormat;
    FOptions: IdxImporterOptions;
  public
    constructor Create(ADocumentFormat: TdxRichEditDocumentFormat; const AOptions: IdxImporterOptions);

    property DocumentFormat: TdxRichEditDocumentFormat read FDocumentFormat;
    property Options: IdxImporterOptions read FOptions;
  end;

  TdxBeforeImportEvent = procedure(Sender: TObject; Args: TdxBeforeImportEventArgs) of object;
  TdxBeforeImportEventHandler = TdxMulticastMethod<TdxBeforeImportEvent>;

  { TdxImportManagerService }

  TdxImportManagerService<TFormat, TResult> = class abstract(TInterfacedObject,
    IdxImportManagerService<TFormat, TResult>)
  strict private
    FImporters: TDictionary<TFormat, IdxCustomImporter>;
  protected
    procedure RegisterNativeFormats; virtual; abstract;
    property Importers: TDictionary<TFormat, IdxCustomImporter> read FImporters;
  public
    constructor Create;
    destructor Destroy; override;

    //IdxImportManagerService
    function GetImporter(const AFormat: TFormat): IdxImporter<TFormat, TResult>;
    function GetImporters: TList<IdxCustomImporter>;
    procedure RegisterImporter(const AImporter: IdxImporter<TFormat, TResult>);
    procedure UnregisterAllImporters;
    procedure UnregisterImporter(const AImporter: IdxImporter<TFormat, TResult> );
  end;

  { TdxDocumentImportManagerService }

  TdxDocumentImportManagerService = class(TdxImportManagerService<TdxRichEditDocumentFormat, Boolean>,
    IdxDocumentImportManagerService)
  protected
    procedure RegisterNativeFormats; override;
  end;

  { TdxPictureFormatsManagerService }

  TdxPictureFormatsManagerService = class(TdxImportManagerService<TdxOfficeImageFormat, TdxOfficeImageReference>)
  protected
    procedure RegisterNativeFormats; override;
  end;

  { TdxOpenFileDialog }

  TdxOpenFileDialog = class(TOpenDialog)
  private
    function GetCheckFileExists: Boolean;
    function GetCheckPathExists: Boolean;
    function GetDereferenceLinks: Boolean;
    function GetMultiSelect: Boolean;
    function GetOverwritePrompt: Boolean;
    function GetRestoreDirectory: Boolean;
    function GetValidateNames: Boolean;
    procedure SetCheckFileExists(const Value: Boolean);
    procedure SetCheckPathExists(const Value: Boolean);
    procedure SetDereferenceLinks(const Value: Boolean);
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetOverwritePrompt(const Value: Boolean);
    procedure SetRestoreDirectory(const Value: Boolean);
    procedure SetValidateNames(const Value: Boolean);
  public
    property CheckFileExists: Boolean read GetCheckFileExists write SetCheckFileExists;
    property CheckPathExists: Boolean read GetCheckPathExists write SetCheckPathExists;
    property DereferenceLinks: Boolean read GetDereferenceLinks write SetDereferenceLinks;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect;
    property OverwritePrompt: Boolean read GetOverwritePrompt write SetOverwritePrompt;
    property RestoreDirectory: Boolean read GetRestoreDirectory write SetRestoreDirectory;
    property ValidateNames: Boolean read GetValidateNames write SetValidateNames;
  end;

  { TdxImportSource }

  TdxImportSource<TFormat, TResult> = class
  strict private
    FFileName: string;
    FImporter: IdxImporter<TFormat, TResult>;
  public
    constructor Create(const AFileName: string; const AImporter: IdxImporter<TFormat, TResult>); overload;
    constructor Create(const AStorage: string; const AFileName: string; const AImporter: IdxImporter<TFormat, TResult>); overload;
    function GetStream: TStream; virtual;

    property FileName: string read FFileName;
    property Importer: IdxImporter<TFormat, TResult> read FImporter;
    property Storage: string read FFileName;
  end;

  { TdxImportHelper }

  TdxImportHelper<TFormat, TResult> = class abstract(TdxImportExportHelper)
  protected
    procedure ApplyEncoding(const AOptions: IdxImporterOptions; AEncoding: TEncoding); virtual; abstract;
    function ChooseImporter(const AFileName: string; AFilterIndex: Integer; AImporters: TList<IdxCustomImporter>; AUseFormatFallback: Boolean): IdxImporter<TFormat, TResult>;
    function ChooseImporterByFileName(const AFileName: string; AImporters: TList<IdxCustomImporter>): IdxImporter<TFormat, TResult>;
    function ChooseImporterByFilterIndex(AFilterIndex: Integer; AImporters: TList<IdxCustomImporter>): IdxImporter<TFormat, TResult>;
    function ChooseImporterByFormat(AFormat: TFormat; AImporters: TList<IdxCustomImporter>): IdxImporter<TFormat, TResult>;
    function CreateImportFilters(AFilters: TdxFileDialogFilterCollection): TdxFileDialogFilterCollection; virtual;
    function CreateOpenFileDialog(AFilters: TdxFileDialogFilterCollection): TdxOpenFileDialog; virtual;
    function GetFallbackFormat: TFormat; virtual; abstract;
    function GetFileName(ADialog: TdxOpenFileDialog): string;
    function GetFileStorage(ADialog: TdxOpenFileDialog): string;
    function GetLoadDocumentDialogFileFilters(const AImportManagerService: IdxImportManagerService<TFormat, TResult>): TdxFileDialogFilterCollection; virtual;
    function GetPredefinedOptions(AFormat: TFormat): TObject; virtual; abstract;
    function GetUndefinedFormat: TFormat; virtual; abstract;
    function EqualsFormat(const Value1, Value2: TFormat): Boolean; virtual; abstract;
    function ShowOpenFileDialog(ADialog: TdxOpenFileDialog; const AParent: TWinControl): Boolean; virtual;

    property UndefinedFormat: TFormat read GetUndefinedFormat;
    property FallbackFormat: TFormat read GetFallbackFormat;
  public
    function Import(AStream: TStream; AFormat: TFormat; const ASourceUri: string;
      const AImportManagerService: IdxImportManagerService<TFormat, TResult>; AEncoding: TEncoding = nil): TResult;
    function AutodetectImporter(const AFileName: string; const AImportManagerService: IdxImportManagerService<TFormat, TResult>;
      AUseFormatFallback: Boolean = True): IdxImporter<TFormat, TResult>;
    function ImportFromFileAutodetectFormat(const AFileName: string;
      const AImportManagerService: IdxImportManagerService<TFormat, TResult>): TResult;
    function InvokeImportDialog(const AParent: TWinControl;
      const AImportManagerService: IdxImportManagerService<TFormat, TResult>): TdxImportSource<TFormat, TResult>;
  end;

  { TdxPictureImporterOptions }

  TdxPictureImporterOptions = class abstract(TcxIUnknownObject, IdxImporterOptions)
  strict private
    FSourceUri: string;
  protected
    //IdxImporterOptions
    function GetSourceUri: string;
    procedure SetSourceUri(const Value: string);
  public
    procedure CopyFrom(const Source: TObject);
    property SourceUri: string read GetSourceUri write SetSourceUri;
  end;

  { TdxPictureImporter }

  TdxPictureImporter = class abstract(TInterfacedObject, IdxImporter<TdxOfficeImageFormat, TdxOfficeImageReference>)
  protected
    //IdxImporter
    function Filter: TdxFileDialogFilter; virtual; abstract;
    function Format: TdxOfficeImageFormat; virtual; abstract;
    function LoadDocument(const ADocumentModel: TdxCustomDocumentModel; AStream: TStream;
      const AOptions: IdxImporterOptions): TdxOfficeImageReference;
    function SetupLoading: TObject{IdxImporterOptions}; virtual; abstract;
  end;

  { TdxDocumentModelImporter }

  TdxDocumentModelImporter = class abstract
  private
    FDocumentModel: TdxCustomDocumentModel;
    FOptions: IdxImporterOptions;
    FUpdateFields: Boolean;
    FProgressIndication: TdxProgressIndication;
    function GetUnitConverter: TdxDocumentModelUnitConverter;
  protected
    procedure ImportCore(AStream: TStream); virtual; abstract;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxImporterOptions); virtual;
    destructor Destroy; override;

    procedure Import(AStream: TStream); overload; virtual;
    procedure Import(const AFileName: string); overload; virtual;

    class procedure ThrowInvalidFile; virtual; {$IFDEF DELPHIXE2}abstract;{$ENDIF}

    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
    property Options: IdxImporterOptions read FOptions;
    property ProgressIndication: TdxProgressIndication read FProgressIndication;
    property UnitConverter: TdxDocumentModelUnitConverter read  GetUnitConverter;
    property UpdateFields: Boolean read FUpdateFields write FUpdateFields;
  end;

implementation

uses
  IOUtils,

  dxCore,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.Import.Formats,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs,
  dxRichEdit.Strs;

type
  { TdxPNGPictureImporter }

  TdxPNGPictureImporterOptions = class(TdxPictureImporterOptions);

  TdxPNGPictureImporter = class(TdxPictureImporter)
  strict private
    class var FFilter: TdxFileDialogFilter;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function Filter: TdxFileDialogFilter; override;
    function Format: TdxOfficeImageFormat; override;
    function SetupLoading: TObject{IdxImporterOptions}; override;
  end;

  { TdxJPEGPictureImporter }

  TdxJPEGPictureImporterOptions = class(TdxPictureImporterOptions);

  TdxJPEGPictureImporter = class(TdxPictureImporter)
  strict private
    class var FFilter: TdxFileDialogFilter;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function Filter: TdxFileDialogFilter; override;
    function Format: TdxOfficeImageFormat; override;
    function SetupLoading: TObject{IdxImporterOptions}; override;
  end;

  { TdxGifPictureImporter }

  TdxGifPictureImporterOptions = class(TdxPictureImporterOptions);

  TdxGifPictureImporter = class(TdxPictureImporter)
  strict private
    class var FFilter: TdxFileDialogFilter;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function Filter: TdxFileDialogFilter; override;
    function Format: TdxOfficeImageFormat; override;
    function SetupLoading: TObject{IdxImporterOptions}; override;
  end;

  { TdxTiffPictureImporter }

  TdxTiffPictureImporterOptions = class(TdxPictureImporterOptions);

  TdxTiffPictureImporter = class(TdxPictureImporter)
  strict private
    class var FFilter: TdxFileDialogFilter;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function Filter: TdxFileDialogFilter; override;
    function Format: TdxOfficeImageFormat; override;
    function SetupLoading: TObject{IdxImporterOptions}; override;
  end;

  { TdxEmfPictureImporter }

  TdxEmfPictureImporterOptions = class(TdxPictureImporterOptions);

  TdxEmfPictureImporter = class(TdxPictureImporter)
  strict private
    class var FFilter: TdxFileDialogFilter;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function Filter: TdxFileDialogFilter; override;
    function Format: TdxOfficeImageFormat; override;
    function SetupLoading: TObject{IdxImporterOptions}; override;
  end;

  { TdxWmfPictureImporter }

  TdxWmfPictureImporterOptions = class(TdxPictureImporterOptions);

  TdxWmfPictureImporter = class(TdxPictureImporter)
  strict private
    class var FFilter: TdxFileDialogFilter;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function Filter: TdxFileDialogFilter; override;
    function Format: TdxOfficeImageFormat; override;
    function SetupLoading: TObject{IdxImporterOptions}; override;
  end;

  { TdxBitmapPictureImporter }

  TdxBitmapPictureImporterOptions = class(TdxPictureImporterOptions);

  TdxBitmapPictureImporter = class(TdxPictureImporter)
  strict private
    class var FFilter: TdxFileDialogFilter;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function Filter: TdxFileDialogFilter; override;
    function Format: TdxOfficeImageFormat; override;
    function SetupLoading: TObject{IdxImporterOptions}; override;
  end;

{ TdxPNGPictureImporter }

class constructor TdxPNGPictureImporter.Initialize;
begin
  FFilter := TdxFileDialogFilter.Create(cxGetResourceString(@sdxRichEditFileFilterDescription_PNGFiles), 'png');
end;

class destructor TdxPNGPictureImporter.Finalize;
begin
  FreeAndNil(FFilter);
end;

function TdxPNGPictureImporter.Filter: TdxFileDialogFilter;
begin
  Result := FFilter;
end;

function TdxPNGPictureImporter.Format: TdxOfficeImageFormat;
begin
  Result := TdxOfficeImageFormat.Png;
end;

function TdxPNGPictureImporter.SetupLoading: TObject;
begin
  Result := TdxPNGPictureImporterOptions.Create;
end;

{ TdxJPEGPictureImporter }

class constructor TdxJPEGPictureImporter.Initialize;
begin
  FFilter := TdxFileDialogFilter.Create(cxGetResourceString(@sdxRichEditFileFilterDescription_JPEGFiles), TArray<string>.Create('jpg', 'jpeg'));
end;

class destructor TdxJPEGPictureImporter.Finalize;
begin
  FreeAndNil(FFilter);
end;

function TdxJPEGPictureImporter.Filter: TdxFileDialogFilter;
begin
  Result := FFilter;
end;

function TdxJPEGPictureImporter.Format: TdxOfficeImageFormat;
begin
  Result := TdxOfficeImageFormat.Jpeg;
end;

function TdxJPEGPictureImporter.SetupLoading: TObject;
begin
  Result := TdxJPEGPictureImporterOptions.Create;
end;

{ TdxGifPictureImporter }

class constructor TdxGifPictureImporter.Initialize;
begin
  FFilter := TdxFileDialogFilter.Create(cxGetResourceString(@sdxRichEditFileFilterDescription_GifFiles), 'gif');
end;

class destructor TdxGifPictureImporter.Finalize;
begin
  FreeAndNil(FFilter);
end;

function TdxGifPictureImporter.Filter: TdxFileDialogFilter;
begin
  Result := FFilter;
end;

function TdxGifPictureImporter.Format: TdxOfficeImageFormat;
begin
  Result := TdxOfficeImageFormat.Gif;
end;

function TdxGifPictureImporter.SetupLoading: TObject;
begin
  Result := TdxGifPictureImporterOptions.Create;
end;

{ TdxTiffPictureImporter }

class constructor TdxTiffPictureImporter.Initialize;
begin
  FFilter := TdxFileDialogFilter.Create(cxGetResourceString(@sdxRichEditFileFilterDescription_TiffFiles), TArray<string>.Create('tif', 'tiff'));
end;

class destructor TdxTiffPictureImporter.Finalize;
begin
  FreeAndNil(FFilter);
end;

function TdxTiffPictureImporter.Filter: TdxFileDialogFilter;
begin
  Result := FFilter;
end;

function TdxTiffPictureImporter.Format: TdxOfficeImageFormat;
begin
  Result := TdxOfficeImageFormat.Tiff;
end;

function TdxTiffPictureImporter.SetupLoading: TObject;
begin
  Result := TdxTiffPictureImporterOptions.Create;
end;

{ TdxEmfPictureImporter }

class constructor TdxEmfPictureImporter.Initialize;
begin
  FFilter := TdxFileDialogFilter.Create(cxGetResourceString(@sdxRichEditFileFilterDescription_EmfFiles), 'emf');
end;

class destructor TdxEmfPictureImporter.Finalize;
begin
  FreeAndNil(FFilter);
end;

function TdxEmfPictureImporter.Filter: TdxFileDialogFilter;
begin
  Result := FFilter;
end;

function TdxEmfPictureImporter.Format: TdxOfficeImageFormat;
begin
  Result := TdxOfficeImageFormat.Emf;
end;

function TdxEmfPictureImporter.SetupLoading: TObject;
begin
  Result := TdxEmfPictureImporterOptions.Create;
end;

{ TdxWmfPictureImporter }

class constructor TdxWmfPictureImporter.Initialize;
begin
  FFilter := TdxFileDialogFilter.Create(cxGetResourceString(@sdxRichEditFileFilterDescription_WmfFiles), 'wmf');
end;

class destructor TdxWmfPictureImporter.Finalize;
begin
  FreeAndNil(FFilter);
end;

function TdxWmfPictureImporter.Filter: TdxFileDialogFilter;
begin
  Result := FFilter;
end;

function TdxWmfPictureImporter.Format: TdxOfficeImageFormat;
begin
  Result := TdxOfficeImageFormat.Wmf;
end;

function TdxWmfPictureImporter.SetupLoading: TObject;
begin
  Result := TdxWmfPictureImporterOptions.Create;
end;

{ TdxBitmapPictureImporter }

class constructor TdxBitmapPictureImporter.Initialize;
begin
  FFilter := TdxFileDialogFilter.Create(cxGetResourceString(@sdxRichEditFileFilterDescription_BitmapFiles), TArray<string>.Create('bmp', 'dib'));
end;

class destructor TdxBitmapPictureImporter.Finalize;
begin
  FreeAndNil(FFilter);
end;

function TdxBitmapPictureImporter.Filter: TdxFileDialogFilter;
begin
  Result := FFilter;
end;

function TdxBitmapPictureImporter.Format: TdxOfficeImageFormat;
begin
  Result := TdxOfficeImageFormat.Bmp;
end;

function TdxBitmapPictureImporter.SetupLoading: TObject;
begin
  Result := TdxBitmapPictureImporterOptions.Create;
end;

{ TdxBeforeImportEventArgs }

constructor TdxBeforeImportEventArgs.Create(ADocumentFormat: TdxRichEditDocumentFormat; const AOptions: IdxImporterOptions);
begin
  FDocumentFormat := ADocumentFormat;
  FOptions := AOptions;
end;

{ TdxImportManagerService<TFormat, TResult> }

constructor TdxImportManagerService<TFormat, TResult>.Create;
begin
  inherited Create;
  FImporters := TObjectDictionary<TFormat, IdxCustomImporter>.Create;
  RegisterNativeFormats;
end;

destructor TdxImportManagerService<TFormat, TResult>.Destroy;
begin
  FreeAndNil(FImporters);
  inherited Destroy;
end;

function TdxImportManagerService<TFormat, TResult>.GetImporter(
  const AFormat: TFormat): IdxImporter<TFormat, TResult>;
var
  AResult: IdxCustomImporter;
begin
  if not Importers.TryGetValue(AFormat, AResult) then
    Result := nil
  else
    Result := IdxImporter<TFormat, TResult>(AResult);
end;

function TdxImportManagerService<TFormat, TResult>.GetImporters: TList<IdxCustomImporter>;
var
  AIntf: IdxCustomImporter;
begin
  Result := TList<IdxCustomImporter>.Create;
  for AIntf in Importers.Values do
    Result.Add(AIntf);
end;

procedure TdxImportManagerService<TFormat, TResult>.RegisterImporter(
  const AImporter: IdxImporter<TFormat, TResult>);
begin
  FImporters.Add(AImporter.Format, AImporter);
end;

procedure TdxImportManagerService<TFormat, TResult>.UnregisterAllImporters;
begin
  FImporters.Clear;
end;

procedure TdxImportManagerService<TFormat, TResult>.UnregisterImporter(
  const AImporter: IdxImporter<TFormat, TResult>);
begin
  if AImporter <> nil then
    Importers.Remove(AImporter.Format);
end;

{ TdxDocumentImportManagerService }

procedure TdxDocumentImportManagerService.RegisterNativeFormats;
begin
  TdxImportFileFormats.RegisterDocumentImportFormats(Self);
end;

{ TdxOpenFileDialog }

function TdxOpenFileDialog.GetCheckFileExists: Boolean;
begin
  Result := ofFileMustExist in Options;
end;

function TdxOpenFileDialog.GetCheckPathExists: Boolean;
begin
  Result := ofPathMustExist in Options;
end;

function TdxOpenFileDialog.GetDereferenceLinks: Boolean;
begin
  Result := not (ofNoDereferenceLinks in Options);
end;

function TdxOpenFileDialog.GetMultiSelect: Boolean;
begin
  Result := ofAllowMultiSelect in Options;
end;

function TdxOpenFileDialog.GetOverwritePrompt: Boolean;
begin
  Result := ofOverwritePrompt in Options;
end;

function TdxOpenFileDialog.GetRestoreDirectory: Boolean;
begin
  Result := False;
end;

function TdxOpenFileDialog.GetValidateNames: Boolean;
begin
  Result := not (ofNoValidate in Options);
end;

procedure TdxOpenFileDialog.SetCheckFileExists(const Value: Boolean);
begin
  if Value then
    Options := Options + [ofFileMustExist]
  else
    Options := Options - [ofFileMustExist];
end;

procedure TdxOpenFileDialog.SetCheckPathExists(const Value: Boolean);
begin
  if Value then
    Options := Options + [ofPathMustExist]
  else
    Options := Options - [ofPathMustExist];
end;

procedure TdxOpenFileDialog.SetDereferenceLinks(const Value: Boolean);
begin
  if Value then
    Options := Options - [ofNoDereferenceLinks]
  else
    Options := Options + [ofNoDereferenceLinks];
end;

procedure TdxOpenFileDialog.SetMultiSelect(const Value: Boolean);
begin
  if Value then
    Options := Options + [ofAllowMultiSelect]
  else
    Options := Options - [ofAllowMultiSelect];
end;

procedure TdxOpenFileDialog.SetOverwritePrompt(const Value: Boolean);
begin
  if Value then
    Options := Options + [ofOverwritePrompt]
  else
    Options := Options - [ofOverwritePrompt];
end;

procedure TdxOpenFileDialog.SetRestoreDirectory(const Value: Boolean);
begin
end;

procedure TdxOpenFileDialog.SetValidateNames(const Value: Boolean);
begin
  if Value then
    Options := Options - [ofNoValidate]
  else
    Options := Options + [ofNoValidate];
end;

{ TdxImportSource }

constructor TdxImportSource<TFormat, TResult>.Create(const AFileName: string;
  const AImporter: IdxImporter<TFormat, TResult>);
begin
  Create(AFileName, AFileName, AImporter);
end;

constructor TdxImportSource<TFormat, TResult>.Create(const AStorage: string;
  const AFileName: string; const AImporter: IdxImporter<TFormat, TResult>);
begin
  inherited Create;
  FImporter := AImporter;
  FFileName := AFileName;
end;

function TdxImportSource<TFormat, TResult>.GetStream: TStream;
begin
  Result := TdxMemoryStream.Create(FileName, fmShareDenyNone or fmOpenRead);
end;

{ TdxImportHelper }

function TdxImportHelper<TFormat, TResult>.Import(AStream: TStream; AFormat: TFormat;
  const ASourceUri: string; const AImportManagerService: IdxImportManagerService<TFormat, TResult>;
  AEncoding: TEncoding): TResult;
var
  AImporter: IdxImporter<TFormat, TResult>;
  APredefinedOptions: TObject;
  AImporterOptions: IdxImporterOptions;
  AOptions: TObject;
begin
  AImporter := AImportManagerService.GetImporter(AFormat);
  if AImporter = nil then
    ThrowUnsupportedFormatException;
  AOptions := AImporter.SetupLoading;
  try
    Supports(AOptions, IdxImporterOptions, AImporterOptions);
    APredefinedOptions := GetPredefinedOptions(AFormat);
    if APredefinedOptions <> nil then
      AImporterOptions.CopyFrom(TObject(APredefinedOptions));
    AImporterOptions.SourceUri := ASourceUri;
    if AEncoding <> nil then
      ApplyEncoding(AImporterOptions, AEncoding);
    Result := AImporter.LoadDocument(DocumentModel, AStream, AImporterOptions);
  finally
    AImporterOptions := nil;
    FreeAndNil(AOptions);
  end;
end;

function TdxImportHelper<TFormat, TResult>.AutodetectImporter(const AFileName: string;
  const AImportManagerService: IdxImportManagerService<TFormat, TResult>;
  AUseFormatFallback: Boolean = True): IdxImporter<TFormat, TResult>;
var
  AImporters: TList<IdxCustomImporter>;
begin
  AImporters := AImportManagerService.GetImporters;
  try
    if AImporters.Count <= 0 then
      Exit(nil);

    Result := ChooseImporter(AFileName, -1, AImporters, AUseFormatFallback);
  finally
    AImporters.Free;
  end;
end;

function TdxImportHelper<TFormat, TResult>.ImportFromFileAutodetectFormat(const AFileName: string; const AImportManagerService: IdxImportManagerService<TFormat, TResult>): TResult;
var
  AImporter: IdxImporter<TFormat, TResult>;
  AStream: TdxMemoryStream;
begin
  AImporter := AutodetectImporter(AFileName, AImportManagerService);
  if AImporter = nil then
    Exit(Default(TResult));

  AStream := TdxMemoryStream.Create(AFileName, fmShareDenyNone or fmOpenRead);
  try
    Result := Import(AStream, AImporter.Format, AFileName, AImportManagerService);
  finally
    AStream.Free;
  end;
end;

function TdxImportHelper<TFormat, TResult>.InvokeImportDialog(const AParent: TWinControl;
  const AImportManagerService: IdxImportManagerService<TFormat, TResult>): TdxImportSource<TFormat, TResult>;
var
  AImporters: TList<IdxCustomImporter>;
  AFilters: TdxFileDialogFilterCollection;
  ADialog: TdxOpenFileDialog;
  AImporter: IdxImporter<TFormat, TResult>;
  AFilterCollection: TdxFileDialogFilterCollection;
begin
  if AImportManagerService = nil then
    ThrowUnsupportedFormatException;

  AImporters := AImportManagerService.GetImporters;
  try
    if AImporters.Count <= 0 then
      ThrowUnsupportedFormatException;

    AFilterCollection := GetLoadDocumentDialogFileFilters(AImportManagerService);
    try
      AFilters := CreateImportFilters(AFilterCollection);
      try
        ADialog := CreateOpenFileDialog(AFilters);
        try
          if not ShowOpenFileDialog(ADialog, AParent) then
            Exit(nil);
          AParent.Repaint;
          AImporter := ChooseImporter(GetFileName(ADialog), ADialog.FilterIndex - 1, AImporters, True);
          if AImporter = nil then
            ThrowUnsupportedFormatException;
          Result := TdxImportSource<TFormat, TResult>.Create(GetFileStorage(ADialog), GetFileName(ADialog), AImporter);
        finally
          ADialog.Free;
        end;
      finally
        AFilters.Free;
      end;
    finally
      AFilterCollection.Free;
    end;
  finally
    AImporters.Free;
  end;
end;

function TdxImportHelper<TFormat, TResult>.ShowOpenFileDialog(ADialog: TdxOpenFileDialog;
  const AParent: TWinControl): Boolean;
begin
  Result := ADialog.Execute;
end;

function TdxImportHelper<TFormat, TResult>.GetLoadDocumentDialogFileFilters(const AImportManagerService: IdxImportManagerService<TFormat, TResult>): TdxFileDialogFilterCollection;
var
  AImporters: TList<IdxCustomImporter>;
  AResult: TdxFileDialogFilterCollection;
  AImporter: IdxImporter<TFormat, TResult>;
  I: Integer;
begin
  AImporters := AImportManagerService.GetImporters;
  try
    Result := TdxFileDialogFilterCollection.Create;
    for I := 0 to AImporters.Count - 1 do
    begin
      AImporter := IdxImporter<TFormat, TResult>(AImporters[I]);
      Result.Add(AImporter.Filter.Clone);
    end;
  finally
    AImporters.Free;
  end;
end;

function TdxImportHelper<TFormat, TResult>.ChooseImporter(const AFileName: string; AFilterIndex: Integer;
  AImporters: TList<IdxCustomImporter>; AUseFormatFallback: Boolean): IdxImporter<TFormat, TResult>;
var
  AFormat: TFormat;
begin
  Result := ChooseImporterByFileName(AFileName, AImporters);
  if Result = nil then
    Result := ChooseImporterByFilterIndex(AFilterIndex, AImporters);
  if Result = nil then
  begin
    if AUseFormatFallback then
      AFormat := FallbackFormat
    else
      AFormat := UndefinedFormat;
    Result := ChooseImporterByFormat(AFormat, AImporters);
  end;
end;

function TdxImportHelper<TFormat, TResult>.ChooseImporterByFileName(const AFileName: string;
  AImporters: TList<IdxCustomImporter>): IdxImporter<TFormat, TResult>;
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
  ACount := AImporters.Count;
  for I := 0 to ACount - 1 do
  begin
    AExtensions := AImporters[I].Filter.Extensions;
    if AExtensions.IndexOf(AExtension) >= 0 then
    begin
      Result := IdxImporter<TFormat, TResult>(AImporters[I]);
      Break;
    end;
  end;
end;

function TdxImportHelper<TFormat, TResult>.ChooseImporterByFormat(AFormat: TFormat;
  AImporters: TList<IdxCustomImporter>): IdxImporter<TFormat, TResult>;
var
  ACount, I: Integer;
begin
  Result := nil;
  if EqualsFormat(AFormat, UndefinedFormat) then
    Exit;
  ACount := AImporters.Count;
  for I := 0 to ACount - 1 do
  begin
    if EqualsFormat(AFormat, IdxImporter<TFormat, TResult>(AImporters[I]).Format) then
    begin
      Result := IdxImporter<TFormat, TResult>(AImporters[I]);
      Break;
    end;
  end;
end;

function TdxImportHelper<TFormat, TResult>.ChooseImporterByFilterIndex(AFilterIndex: Integer; AImporters: TList<IdxCustomImporter>): IdxImporter<TFormat, TResult>;
begin
  if (AFilterIndex >= 0) and (AFilterIndex < AImporters.Count) then
    Result := IdxImporter<TFormat, TResult>(AImporters[AFilterIndex])
  else
    Result := nil;
end;

function TdxImportHelper<TFormat, TResult>.CreateOpenFileDialog(AFilters: TdxFileDialogFilterCollection): TdxOpenFileDialog;
begin
  Result := TdxOpenFileDialog.Create(nil);
  Result.Filter := CreateFilterString(AFilters);
  Result.FilterIndex := 2;
  Result.MultiSelect := False;

  Result.RestoreDirectory := True;
  Result.CheckFileExists := True;
  Result.CheckPathExists := True;

  Result.DereferenceLinks := True;
  Result.ValidateNames := True;
end;

function TdxImportHelper<TFormat, TResult>.CreateImportFilters(AFilters: TdxFileDialogFilterCollection): TdxFileDialogFilterCollection;
var
  AAllSupportedFilesFilter, AFilter: TdxFileDialogFilter;
  ACount, I: Integer;
begin
  AAllSupportedFilesFilter := TdxFileDialogFilter.Create;
  AAllSupportedFilesFilter.Description := cxGetResourceString(@sdxRichEditFileFilterDescription_AllFiles);

  Result := TdxFileDialogFilterCollection.Create;
  Result.Add(TdxFileDialogFilter.AllFiles.Clone);
  Result.Add(AAllSupportedFilesFilter);

  ACount := AFilters.Count;
  for I := 0 to ACount - 1 do
  begin
    AFilter := AFilters[I];
    if AFilter.Extensions.Count > 0 then
    begin
      Result.Add(AFilter.Clone);
      AAllSupportedFilesFilter.Extensions.AddStrings(AFilter.Extensions);
    end;
  end;
end;

function TdxImportHelper<TFormat, TResult>.GetFileStorage(ADialog: TdxOpenFileDialog): string;
begin
  Result := ADialog.FileName;
end;

function TdxImportHelper<TFormat, TResult>.GetFileName(ADialog: TdxOpenFileDialog): string;
begin
  Result := ADialog.FileName;
end;

{ TdxPictureFormatsManagerService }

procedure TdxPictureFormatsManagerService.RegisterNativeFormats;
begin
  RegisterImporter(TdxBitmapPictureImporter.Create);
  RegisterImporter(TdxJPEGPictureImporter.Create);
  RegisterImporter(TdxPNGPictureImporter.Create);
  RegisterImporter(TdxGifPictureImporter.Create);
  RegisterImporter(TdxTiffPictureImporter.Create);
  RegisterImporter(TdxEmfPictureImporter.Create);
  RegisterImporter(TdxWmfPictureImporter.Create);
end;

{ TdxPictureImporterOptions }

procedure TdxPictureImporterOptions.CopyFrom(const Source: TObject);
begin
//do nothing
end;

function TdxPictureImporterOptions.GetSourceUri: string;
begin
  Result := FSourceUri;
end;

procedure TdxPictureImporterOptions.SetSourceUri(const Value: string);
begin
  FSourceUri := Value;
end;

{ TdxPictureImporter }

function TdxPictureImporter.LoadDocument(
  const ADocumentModel: TdxCustomDocumentModel; AStream: TStream;
  const AOptions: IdxImporterOptions): TdxOfficeImageReference;
begin
  Result := TdxSimpleDocumentModel(ADocumentModel).CreateImage(AStream);
end;

{ TdxDocumentModelImporter }

constructor TdxDocumentModelImporter.Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxImporterOptions);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
  FOptions := AOptions;
  FProgressIndication := TdxProgressIndication.Create(ADocumentModel);
end;

destructor TdxDocumentModelImporter.Destroy;
begin
  FProgressIndication.Free;
  FOptions := nil;
  inherited Destroy;
end;

{$IFNDEF DELPHIXE2}
class procedure TdxDocumentModelImporter.ThrowInvalidFile;
begin
  raise Exception.Create('for C++Builder XE');
end;
{$ENDIF}

procedure TdxDocumentModelImporter.Import(AStream: TStream);
begin
  if AStream = nil then
  begin
    TdxRichEditExceptions.ThrowArgumentException('stream', AStream);
    Exit;
  end;

  ProgressIndication.&Begin(cxGetResourceString(@sdxRichEditMsg_Loading), AStream.Position, AStream.Size - AStream.Position, AStream.Position);
  try
    ImportCore(AStream);
  finally
    ProgressIndication.&End;
  end;
end;

procedure TdxDocumentModelImporter.Import(const AFileName: string);
var
  AStream: TStream;
begin
  AStream := TdxMemoryStream.Create(AFileName, fmShareDenyNone);
  try
    AStream.Position := 0;
    Import(AStream);
  finally
    AStream.Free;
  end;
end;

function TdxDocumentModelImporter.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := DocumentModel.UnitConverter;
end;

end.
