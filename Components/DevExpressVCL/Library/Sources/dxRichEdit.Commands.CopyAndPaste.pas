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

unit dxRichEdit.Commands.CopyAndPaste;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, dxRichEdit.DocumentModel.Core,
  Shlobj,

  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Commands.MultiCommand,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Control.HitTest,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.DocumentModel.Commands,
  dxRichEdit.Commands,
  dxRichEdit.View.Core,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands.Insert,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.Types;

type
  { TdxPasteContentCommandBase }

  TdxPasteContentCommandBase = class abstract(TdxInsertObjectCommandBase)
  private
    FPasteSource: IdxPasteSource;
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    function GetFileDropListData: TArray<string>;
    function GetFormat: TdxRichEditDocumentFormat; virtual; abstract;
    function GetPasteSource: IdxPasteSource; virtual;
    function IsDataAvailable: Boolean; virtual; abstract;
    procedure SetPasteSource(const Value: IdxPasteSource); virtual;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property Format: TdxRichEditDocumentFormat read GetFormat;
    property PasteSource: IdxPasteSource read GetPasteSource write SetPasteSource;
  end;
  TdxPasteContentCommandBaseClass = class of TdxPasteContentCommandBase;

  { TdxPasteContentConvertedToDocumentModelCommandBase }

  TdxPasteContentConvertedToDocumentModelCommandBase = class(TdxPasteContentCommandBase)
  private
    FInnerCommand: TdxPieceTablePasteContentConvertedToDocumentModelCommandBase;
  protected
    function GetFormat: TdxRichEditDocumentFormat; override;
    function GetPasteSource: IdxPasteSource; override;
    procedure SetPasteSource(const Value: IdxPasteSource); override;

    procedure CreateInnerCommand;
    function CreateInnerCommandCore: TdxPieceTablePasteContentConvertedToDocumentModelCommandBase; virtual; abstract;
    procedure ModifyModel; override;
    function IsDataAvailable: Boolean; override;

    property InnerCommand: TdxPieceTablePasteContentConvertedToDocumentModelCommandBase read FInnerCommand;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    destructor Destroy; override;
  end;

  { TdxPasteRtfTextCommand }

  TdxPasteRtfTextCommand = class(TdxPasteContentConvertedToDocumentModelCommandBase)
  protected
    function CreateInnerCommandCore: TdxPieceTablePasteContentConvertedToDocumentModelCommandBase; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxPastePlainTextCommand }

  TdxPastePlainTextCommand = class(TdxPasteContentCommandBase)
  protected
    function GetFormat: TdxRichEditDocumentFormat; override;
    function IsDataAvailable: Boolean; override;
    procedure ModifyModel; override;

    function GetTextData: string;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxPasteImageCommand }

  TdxPasteImageCommand = class(TdxPasteContentCommandBase)
  protected
    function GetFormat: TdxRichEditDocumentFormat; override;
    procedure ModifyModel; override;
    function IsDataAvailable: Boolean; override;

    function Image: TdxOfficeImage; virtual;
    function CalculateImageScale(AImage: TdxOfficeImage; const AColumnBounds: TRect): Integer; virtual;
    function GetCurrentColumnBounds: TRect; overload; virtual;
    class function GetCurrentColumnBounds(ACaretPosition: TdxCaretPosition): TRect; overload; virtual;
    function GetImageScale(const AImageSizeInLayoutUnits: TSize; const AColumnBounds: TRect): Integer; virtual;

    function InsertPicture(AImage: TdxOfficeImage): Boolean; overload; virtual;
    function InsertPicture(AImage: TdxOfficeImage; const AColumnBounds: TRect): Boolean; overload; virtual;
    procedure InsertPictureCore(AImage: TdxOfficeImage; AScaleX, AScaleY: Integer);
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxPasteMetafileCommand }

  TdxPasteMetafileCommand = class(TdxPasteImageCommand)
  protected
    function IsDataAvailable: Boolean; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxPasteImagesFromFilesCommand }

  TdxPasteImagesFromFilesCommand = class(TdxPasteContentCommandBase)
  protected
    function GetFormat: TdxRichEditDocumentFormat; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function IsDataAvailable: Boolean; override;
    procedure ModifyModel; override;
    procedure PasteImageFromFile(const AFileName: string; const AColumnBounds: TRect);
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxPasteSelectionCommand }

  TdxPasteSelectionCommand = class(TdxTransactedInsertObjectCommand)
  private
    function GetFormat: TdxRichEditDocumentFormat;
    procedure SetFormat(const Value: TdxRichEditDocumentFormat);
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
    function GetKeepLastParagraphMarkInSelection: Boolean; override;
    function CreateInsertObjectCommand: TdxRichEditCommand; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure UpdateUIState(const AState: IdxCommandUIState); override;

    property Format: TdxRichEditDocumentFormat read GetFormat write SetFormat;
  end;

  { TdxPasteSelectionCoreCommand }

  TdxPasteSelectionCoreCommand = class(TdxMultiCommand)
  private
    FFormat: TdxRichEditDocumentFormat;
    FPasteSource: IdxPasteSource;
    FPasteException: Exception;
    procedure SetFormat(const Value: TdxRichEditDocumentFormat);
  protected
    function GetExecutionMode: TdxMultiCommandExecutionMode; override;
    function GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode; override;
    procedure ForceExecuteCore(const AState: IdxCommandUIState); override;
    function ExecuteCommand(ACommand: TdxCommand;
      const AState: IdxCommandUIState): Boolean; override;
    procedure CreateCommands; override;

    procedure AddCommand(ACommandClass: TdxPasteContentCommandBaseClass);
    procedure AssignPasteSource;
    function CanAddCommandFormat(ACommand: TdxPasteContentCommandBase): Boolean;
    function ContainsData(const AFormat: string): Boolean;
    function PasteFromIE: Boolean;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl; const APasteSource: IdxPasteSource); reintroduce;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property Format: TdxRichEditDocumentFormat read FFormat write SetFormat;
    property PasteSource: IdxPasteSource read FPasteSource;
  end;

  { TdxCutSelectionCommand }

  TdxCutSelectionCommand = class(TdxMultiCommand)
  protected
    procedure CreateCommands; override;
    function GetExecutionMode: TdxMultiCommandExecutionMode; override;
    function GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode; override;
  public
    class function Id: TdxRichEditCommandId; override;
    procedure UpdateUIState(const AState: IdxCommandUIState); override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxPasteDataObjectCoreCommand }

  TdxPasteDataObjectCoreCommand = class(TdxPasteSelectionCoreCommand)
  private
    FPasteSource: TdxDataObjectPasteSource;
    function GetDataObject: IdxDataObject;
    procedure SetDataObject(const Value: IdxDataObject);
  protected
    procedure CreateCommands; override;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl; const ADataObject: IdxDataObject); reintroduce;
    destructor Destroy; override;

    property DataObject: IdxDataObject read GetDataObject write SetDataObject;
  end;

  { TdxPasteLoadDocumentFromFileCommand }

  TdxPasteLoadDocumentFromFileCommand = class(TdxPasteContentCommandBase)
  protected
    function CanLoadFile(const AFileName: string): Boolean; virtual;
  protected
    function GetFormat: TdxRichEditDocumentFormat; override;
    function IsDataAvailable: Boolean; override;
    procedure ModifyModel; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxPasteHtmlTextFromDragDropCommand }

  TdxPasteHtmlTextFromDragDropCommand = class(TdxPasteContentConvertedToDocumentModelCommandBase)
  protected
    function CreateInnerCommandCore: TdxPieceTablePasteContentConvertedToDocumentModelCommandBase; override;
  end;

  { TdxCopyAndSaveContentCommand }

  TdxCopyAndSaveContentCommand = class(TdxRichEditCaretBasedCommand)
  strict private
    FRtfText: string;
    FSuppressStoreImageSizeCollection: string;
  protected
    procedure ExecuteCore; override;
    procedure CopySelectedContent(AManager: TdxCopySelectionManager);
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    property RtfText: string read FRtfText;
    property SuppressStoreImageSizeCollection: string read FSuppressStoreImageSizeCollection;
  end;

  { TdxPasteSavedContentCommand }

  TdxPasteSavedContentCommand = class(TdxPasteRtfTextCommand)
  strict private
    FCopyCommand: TdxCopyAndSaveContentCommand;
  protected
    function CreateInnerCommandCore: TdxPieceTablePasteContentConvertedToDocumentModelCommandBase; override;
  public
    constructor Create(const AControl: IdxRichEditControl; ACopyCommand: TdxCopyAndSaveContentCommand); reintroduce;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxPieceTablePasteSavedContentCommand }

  TdxPieceTablePasteSavedContentCommand = class(TdxPieceTablePasteRtfTextCommand)
  strict private
    FCopyCommand: TdxCopyAndSaveContentCommand;
  protected
    function GetContent: TdxClipboardStringContent; override;
    function GetAdditionalContentString: string; override;
  public
    constructor Create(APieceTable: TdxPieceTable; ACopyCommand: TdxCopyAndSaveContentCommand); reintroduce;
    function IsDataAvailable: Boolean; override;
  end;

  { TdxCopySelectionCommand }

  TdxCopySelectionCommand = class(TdxRichEditCaretBasedCommand)
  strict private
    FCopySelectionManager: TdxCopySelectionManager;
  private
    function GetDefaultPropertiesCopyOptions: TdxDefaultPropertiesCopyOptions;
    function GetFixLastParagraph: Boolean;
    procedure SetDefaultPropertiesCopyOptions(const Value: TdxDefaultPropertiesCopyOptions);
    procedure SetFixLastParagraph(const Value: Boolean);
  protected
    function CreateCopySelectionManager: TdxCopySelectionManager; virtual;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property CopySelectionManager: TdxCopySelectionManager read FCopySelectionManager;
    property DefaultPropertiesCopyOptions: TdxDefaultPropertiesCopyOptions read GetDefaultPropertiesCopyOptions write SetDefaultPropertiesCopyOptions;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl); override;
    destructor Destroy; override;

    procedure ExecuteCore; override;
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    property FixLastParagraph: Boolean read GetFixLastParagraph write SetFixLastParagraph;
  end;

var
  dxPasteHtmlTextCommandClass: TdxPasteContentCommandBaseClass;

implementation

uses
  Contnrs, Clipbrd, Windows, Math, dxCore, cxGeometry, dxTypeHelpers, ShellAPI,

  dxRichEdit.Commands.Images,
  dxRichEdit.Utils.Exceptions.Strs,
  dxRichEdit.Commands.Strs,
  dxRichEdit.Commands.Delete,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.Selections.Core,
  dxRichEdit.DocumentLayout.UnitConverter, dxCoreClasses;

{ TdxPasteContentCommandBase }

constructor TdxPasteContentCommandBase.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FPasteSource := TdxEmptyPasteSource.Create;
end;

class function TdxPasteContentCommandBase.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteDescription);
end;

class function TdxPasteContentCommandBase.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteMenuCaption);
end;

function TdxPasteContentCommandBase.GetFileDropListData: TArray<string>;
var
  ALength: Integer;
  AData: TBytes;
  AFiles: PByte;
  AEnd: PByte;
  AFileCount: Integer;
  ADropFiles: PDropFiles;
  S: string;
begin
  AData := PasteSource.GetData(TdxOfficeDataFormats.FileDrop);
  ALength := Length(AData);
  if ALength > 0 then
  try
    ADropFiles := Pointer(AData);
    AFiles := Pointer(AData);
    Inc(AFiles, ADropFiles.pFiles);
    AEnd := Pointer(AData);
    Inc(AEnd, ALength - 2);
    AFileCount := 0;
    while AFiles < AEnd do
    begin
      S := PChar(AFiles);
      Inc(AFiles, (Length(S) + 1) * SizeOf(Char));
      Inc(AFileCount);
      SetLength(Result, AFileCount);
      Result[AFileCount - 1] := S;
    end;
  finally
    AData := nil;
  end;
end;

function TdxPasteContentCommandBase.GetPasteSource: IdxPasteSource;
begin
  Result := FPasteSource;
end;

procedure TdxPasteContentCommandBase.SetPasteSource(
  const Value: IdxPasteSource);
begin
  FPasteSource := Value;
end;

procedure TdxPasteContentCommandBase.UpdateUIStateCore(
  const AState: IdxCommandUIState);
var
  AEnabled: Boolean;
begin
  CheckExecutedAtUIThread;
  AEnabled := IsContentEditable and IsDataAvailable;
  AState.Enabled := AEnabled;
  AState.Visible := True;
  AState.Checked := False;
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxPasteContentConvertedToDocumentModelCommandBase }

constructor TdxPasteContentConvertedToDocumentModelCommandBase.Create(
  const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  CreateInnerCommand;
end;

procedure TdxPasteContentConvertedToDocumentModelCommandBase.CreateInnerCommand;
begin
  FInnerCommand := CreateInnerCommandCore;
end;

destructor TdxPasteContentConvertedToDocumentModelCommandBase.Destroy;
begin
  FreeAndNil(FInnerCommand);
  inherited Destroy;
end;

function TdxPasteContentConvertedToDocumentModelCommandBase.GetFormat: TdxRichEditDocumentFormat;
begin
  Result := InnerCommand.Format;
end;

function TdxPasteContentConvertedToDocumentModelCommandBase.GetPasteSource: IdxPasteSource;
begin
  Result := InnerCommand.PasteSource;
end;

function TdxPasteContentConvertedToDocumentModelCommandBase.IsDataAvailable: Boolean;
begin
  Result := InnerCommand.IsDataAvailable;
end;

procedure TdxPasteContentConvertedToDocumentModelCommandBase.ModifyModel;
begin
  InnerCommand.ForceInsertFloatingObjectAtParagraphStart := True;
  InnerCommand.Execute;
end;

procedure TdxPasteContentConvertedToDocumentModelCommandBase.SetPasteSource(
  const Value: IdxPasteSource);
begin
  InnerCommand.PasteSource := Value;
end;

{ TdxPasteRtfTextCommand }

function TdxPasteRtfTextCommand.CreateInnerCommandCore: TdxPieceTablePasteContentConvertedToDocumentModelCommandBase;
begin
  Result := TdxPieceTablePasteRtfTextCommand.Create(ActivePieceTable)
end;

class function TdxPasteRtfTextCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteRtfTextDescription);
end;

class function TdxPasteRtfTextCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteRtfTextMenuCaption);
end;

{ TdxPastePlainTextCommand }

class function TdxPastePlainTextCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPastePlainTextDescription);
end;

function TdxPastePlainTextCommand.GetFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.PlainText;
end;

class function TdxPastePlainTextCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPastePlainTextMenuCaption);
end;

function TdxPastePlainTextCommand.GetTextData: string;

  function InternalGetData(const AFormat: string; var AResult: string): Boolean;
  begin
    AResult := PasteSource.GetDataAsText(AFormat);
    Result := Length(AResult) > 0;
  end;

begin
  if not InternalGetData(TdxOfficeDataFormats.UnicodeText, Result) and
    not InternalGetData(TdxOfficeDataFormats.Text, Result) then
    Result := '';
end;

function TdxPastePlainTextCommand.IsDataAvailable: Boolean;
begin
  Result := PasteSource.ContainsData(TdxOfficeDataFormats.UnicodeText) or
    PasteSource.ContainsData(TdxOfficeDataFormats.Text) or
    PasteSource.ContainsData(TdxOfficeDataFormats.OemText);
end;

procedure TdxPastePlainTextCommand.ModifyModel;
var
  AText: string;
  APosition: TdxDocumentLogPosition;
begin
  AText := GetTextData;
  if AText <> '' then
  begin
    APosition := DocumentModel.Selection.&End;
    ActivePieceTable.InsertPlainText(APosition, AText, GetForceVisible);
  end;
end;

{ TdxCutSelectionCommand }

procedure TdxCutSelectionCommand.CreateCommands;
begin
  Commands.Add(TdxCopySelectionCommand.Create(RichEditControl));
  Commands.Add(TdxDeleteNonEmptySelectionCommand.Create(RichEditControl));
end;

function TdxCutSelectionCommand.GetExecutionMode: TdxMultiCommandExecutionMode;
begin
  Result := TdxMultiCommandExecutionMode.ExecuteAllAvailable;
end;

class function TdxCutSelectionCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.CutSelection;
end;

class function TdxCutSelectionCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandCutSelectionDescription);
end;

class function TdxCutSelectionCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandCutSelectionMenuCaption);
end;

class function TdxCutSelectionCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.CutSelection;
end;

procedure TdxCutSelectionCommand.UpdateUIState(const AState: IdxCommandUIState);
begin
  inherited UpdateUIState(AState);
  ApplyCommandRestrictionOnEditableControl(AState, Options.Behavior.Cut, AState.Enabled);
end;

function TdxCutSelectionCommand.GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode;
begin
  Result := TdxMultiCommandUpdateUIStateMode.EnableIfAllAvailable;
end;

{ TdxPasteDataObjectCoreCommand }

constructor TdxPasteDataObjectCoreCommand.Create(
  const ARichEditControl: IdxRichEditControl; const ADataObject: IdxDataObject);
var
  APasteSource: TdxDataObjectPasteSource;
begin
  APasteSource := TdxDataObjectPasteSource.Create(ADataObject);
  inherited Create(ARichEditControl, APasteSource);
  FPasteSource := APasteSource;
end;

procedure TdxPasteDataObjectCoreCommand.CreateCommands;
begin
  AddCommand(TdxPasteLoadDocumentFromFileCommand);
  if PasteFromIE then
  begin
    AddCommand(TdxPasteRtfTextCommand);
  end
  else
  begin
    AddCommand(TdxPasteRtfTextCommand);
  end;
  AddCommand(TdxPastePlainTextCommand);
  AddCommand(TdxPasteImageCommand);
  AddCommand(TdxPasteImagesFromFilesCommand);
end;

destructor TdxPasteDataObjectCoreCommand.Destroy;
begin
  FPasteSource := nil;
  inherited Destroy;
end;

function TdxPasteDataObjectCoreCommand.GetDataObject: IdxDataObject;
begin
  Result := TdxDataObjectPasteSource(PasteSource).DataObject;
end;

procedure TdxPasteDataObjectCoreCommand.SetDataObject(
  const Value: IdxDataObject);
begin
  TdxDataObjectPasteSource(PasteSource).DataObject := Value;
end;

{ TdxPasteImagesFromFilesCommand }

class function TdxPasteImagesFromFilesCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteFilesDescription);
end;

function TdxPasteImagesFromFilesCommand.GetFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Undefined;
end;

class function TdxPasteImagesFromFilesCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteFilesMenuCaption);
end;

function TdxPasteImagesFromFilesCommand.IsDataAvailable: Boolean;
begin
  Result := PasteSource.ContainsData(TdxOfficeDataFormats.FileDrop, True);
end;

procedure TdxPasteImagesFromFilesCommand.ModifyModel;
var
  AFiles: TArray<string>;
  AColumnBounds: TRect;
  I: Integer;
  AFileName: string;
begin
  AFiles := GetFileDropListData;
  if Length(AFiles) = 0 then
    Exit;

  AColumnBounds := TdxPasteImageCommand.GetCurrentColumnBounds(ActiveView.CaretPosition);
  for I := 0 to Length(AFiles) - 1 do
  begin
    AFileName :=  AFiles[I];
    if FileExists(AFileName) then
      PasteImageFromFile(AFileName, AColumnBounds);
  end;
end;

procedure TdxPasteImagesFromFilesCommand.PasteImageFromFile(
  const AFileName: string; const AColumnBounds: TRect);
var
  AImage: TdxOfficeImage;
  ACommand: TdxPasteImageCommand;
begin
  AImage := TdxOfficeImage.Create;
  try
    try
      AImage.LoadFromFile(AFileName);
      AImage.HandleNeeded;
    except
      Exit;
    end;
    if not AImage.Empty then
    begin
      ACommand := TdxPasteImageCommand.Create(RichEditControl);
      try
        ACommand.InsertPicture(AImage, AColumnBounds);
        AImage := nil;
      finally
        ACommand.Free;
      end;
    end;
  finally
    AImage.Free;
  end;
end;

procedure TdxPasteImagesFromFilesCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.InlinePictures, AState.Enabled);
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxPasteLoadDocumentFromFileCommand }

function TdxPasteLoadDocumentFromFileCommand.CanLoadFile(
  const AFileName: string): Boolean;
begin
  Result := DocumentModel.AutodetectDocumentFormat(AFileName, False) <> TdxRichEditDocumentFormat.Undefined;
end;

class function TdxPasteLoadDocumentFromFileCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteFilesDescription);
end;

function TdxPasteLoadDocumentFromFileCommand.GetFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Undefined;
end;

class function TdxPasteLoadDocumentFromFileCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteFilesMenuCaption);
end;

function TdxPasteLoadDocumentFromFileCommand.IsDataAvailable: Boolean;
var
  AFiles: TArray<string>;
begin
  if not PasteSource.ContainsData(TdxOfficeDataFormats.FileDrop, True) then
    Result := False
  else
  begin
    AFiles := GetFileDropListData;
    Result := (Length(AFiles) = 1) and CanLoadFile(AFiles[0]);
  end;
end;

procedure TdxPasteLoadDocumentFromFileCommand.ModifyModel;
var
  AFiles: TArray<string>;
begin
  AFiles := GetFileDropListData;
  if Length(AFiles) <> 1 then
    Exit;
  try
    if InnerControl.RaiseDocumentClosing then
      InnerControl.LoadDocument(AFiles[0]);
  except
  end;
end;

{ TdxPasteHtmlTextFromDragDropCommand }

function TdxPasteHtmlTextFromDragDropCommand.CreateInnerCommandCore: TdxPieceTablePasteContentConvertedToDocumentModelCommandBase;
begin
  Assert(False);
  Result := nil;
end;

{ TdxPasteSavedContentCommand }

constructor TdxPasteSavedContentCommand.Create(
  const AControl: IdxRichEditControl;
  ACopyCommand: TdxCopyAndSaveContentCommand);
begin
  FCopyCommand := ACopyCommand;
  inherited Create(AControl);
end;

function TdxPasteSavedContentCommand.CreateInnerCommandCore: TdxPieceTablePasteContentConvertedToDocumentModelCommandBase;
begin
  if FCopyCommand = nil then
    Result := nil
  else
    Result := TdxPieceTablePasteSavedContentCommand.Create(ActivePieceTable, FCopyCommand);
end;

class function TdxPasteSavedContentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionThrowInternalException);
end;

class function TdxPasteSavedContentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionThrowInternalException);
end;

{ TdxPieceTablePasteSavedContentCommand }

constructor TdxPieceTablePasteSavedContentCommand.Create(
  APieceTable: TdxPieceTable; ACopyCommand: TdxCopyAndSaveContentCommand);
begin
  inherited Create(APieceTable);
  FCopyCommand := ACopyCommand;
end;

function TdxPieceTablePasteSavedContentCommand.GetAdditionalContentString: string;
begin
  Result := FCopyCommand.SuppressStoreImageSizeCollection;
end;

function TdxPieceTablePasteSavedContentCommand.GetContent: TdxClipboardStringContent;
begin
  Result := TdxClipboardStringContent.Create(FCopyCommand.RtfText);
end;

function TdxPieceTablePasteSavedContentCommand.IsDataAvailable: Boolean;
begin
  Result := FCopyCommand.RtfText <> '';
end;

{ TdxCopyAndSaveContentCommand }

procedure TdxCopyAndSaveContentCommand.CopySelectedContent(
  AManager: TdxCopySelectionManager);
var
  ASelection: TdxSelection;
  ASelections: TdxSelectionRangeCollection;
  AOptions: TdxRtfDocumentExporterOptions;
begin
  ASelection := DocumentModel.Selection;
  ASelections := ASelection.GetSortedSelectionCollection;
  try
    AOptions := TdxRtfDocumentExporterOptions.Create;
    try
      AOptions.ExportFinalParagraphMark := TdxExportFinalParagraphMark.Never;
      FRtfText := AManager.GetRtfText(ASelection.PieceTable, ASelections, AOptions, True, True);
      FSuppressStoreImageSizeCollection := AManager.GetSuppressStoreImageSizeCollection(ASelection.PieceTable, ASelections);
    finally
      AOptions.Free;
    end;
  finally
    ASelections.Free;
  end;
end;

procedure TdxCopyAndSaveContentCommand.ExecuteCore;
var
  AManager: TdxCopySelectionManager;
begin
  AManager := InnerControl.CreateCopySelectionManager;
  try
    CopySelectedContent(AManager);
  finally
    AManager.Free;
  end;
end;

class function TdxCopyAndSaveContentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionThrowInternalException);
end;

class function TdxCopyAndSaveContentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionThrowInternalException);
end;

procedure TdxCopyAndSaveContentCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
begin
  AState.Checked := False;
  AState.Enabled := True;
  AState.Visible := True;
end;

{ TdxPasteImageCommand }

function TdxPasteImageCommand.Image: TdxOfficeImage;
var
  AData: TBytes;
  ALength: Integer;
  AHandle: THandle;
begin
  AData := PasteSource.GetData(TdxOfficeDataFormats.Bitmap, True);
  ALength := Length(AData);
  if ALength = SizeOf(AHandle) then
  begin
    Move(AData[0], AHandle, ALength);
    Result := TdxOfficeImage.Create;
    Result.LoadFromClipboardFormat(CF_BITMAP, AHandle, 0);
    if Result.Empty then
      FreeAndNil(Result);
  end
  else
    Result := nil;
end;

function TdxPasteImageCommand.CalculateImageScale(AImage: TdxOfficeImage; const AColumnBounds: TRect): Integer;
var
  ADpiX, ADpiY: Single;
  AUnitConverter: TdxDocumentLayoutUnitConverter;
  AImageWidthInLayoutUnits, AImageHeightInLayoutUnits: Integer;
begin
  ADpiX := IfThen(AImage.RawFormat = TdxOfficeImageFormat.Emf, TdxMetafileHelper.MetafileResolution,  AImage.HorizontalResolution);
  ADpiY := IfThen(AImage.RawFormat = TdxOfficeImageFormat.Emf, TdxMetafileHelper.MetafileResolution, AImage.VerticalResolution);

  AUnitConverter := DocumentModel.LayoutUnitConverter;
  AImageWidthInLayoutUnits := AUnitConverter.PixelsToLayoutUnits(AImage.Width, ADpiX);
  AImageHeightInLayoutUnits := AUnitConverter.PixelsToLayoutUnits(AImage.Height, ADpiY);

  Result := GetImageScale(cxSize(AImageWidthInLayoutUnits, AImageHeightInLayoutUnits), AColumnBounds);
end;

function TdxPasteImageCommand.GetCurrentColumnBounds: TRect;
begin
  Result := GetCurrentColumnBounds(ActiveView.CaretPosition);
end;

class function TdxPasteImageCommand.GetCurrentColumnBounds(
  ACaretPosition: TdxCaretPosition): TRect;
begin
  if ACaretPosition.Update(TdxDocumentLayoutDetailsLevel.Column) then
    Result := ACaretPosition.LayoutPosition.Column.Bounds
  else
    Result.InitSize(0, 0, MaxInt, MaxInt);
end;

class function TdxPasteImageCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteImageDescription);
end;

function TdxPasteImageCommand.GetImageScale(const AImageSizeInLayoutUnits: TSize; const AColumnBounds: TRect): Integer;
var
  AScaleX, AScaleY: Single;
begin
  AScaleX := 1.0;
  AScaleY := 1.0;
  if AImageSizeInLayoutUnits.Width > AColumnBounds.Width then
    AScaleX := AColumnBounds.Width / AImageSizeInLayoutUnits.Width;
  if AImageSizeInLayoutUnits.Height > AColumnBounds.Height then
    AScaleY := AColumnBounds.Height / AImageSizeInLayoutUnits.Height;
  Result := Trunc(100 * Min(AScaleX, AScaleY));
end;

class function TdxPasteImageCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteImageMenuCaption);
end;

function TdxPasteImageCommand.GetFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Undefined;
end;

function TdxPasteImageCommand.InsertPicture(AImage: TdxOfficeImage): Boolean;
var
  ABounds: TRect;
begin
  ABounds := GetCurrentColumnBounds;
  Result := InsertPicture(AImage, ABounds);
end;

function TdxPasteImageCommand.InsertPicture(AImage: TdxOfficeImage;
  const AColumnBounds: TRect): Boolean;
var
  AScale: Integer;
begin
  Result := False;
  if (AImage = nil) or not DocumentModel.DocumentCapabilities.InlinePicturesAllowed then
    Exit;
  AScale := CalculateImageScale(AImage, AColumnBounds);
  InsertPictureCore(AImage, AScale, AScale);
  Result := True;
end;

procedure TdxPasteImageCommand.InsertPictureCore(AImage: TdxOfficeImage;
  AScaleX, AScaleY: Integer);
var
  AImageReference: TdxOfficeImageReference;
begin
  AImageReference := DocumentModel.CreateImage(AImage);
  try
    ActivePieceTable.InsertInlinePicture(DocumentModel.Selection.&End, AImageReference, AScaleX, AScaleY, GetForceVisible);
  finally
    AImageReference.Free;
  end;
end;

function TdxPasteImageCommand.IsDataAvailable: Boolean;
begin
  Result := PasteSource.ContainsData(TdxOfficeDataFormats.Bitmap, True);
end;

procedure TdxPasteImageCommand.ModifyModel;
var
  AImage: TdxOfficeImage;
begin
  AImage := Image;
  if AImage <> nil then
    if not InsertPicture(AImage) then
      AImage.Free;
end;

{ TdxPasteMetafileCommand }

class function TdxPasteMetafileCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteMetafileImageDescription);
end;

class function TdxPasteMetafileCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteMetafileImageMenuCaption);
end;

function TdxPasteMetafileCommand.IsDataAvailable: Boolean;
begin
  Result := PasteSource.ContainsData(TdxOfficeDataFormats.EnhancedMetafile);
end;

{ TdxPasteSelectionCommand }

function TdxPasteSelectionCommand.CreateInsertObjectCommand: TdxRichEditCommand;
var
  ASource: IdxPasteSource;
begin
  ASource := TdxClipboardPasteSource.Create;
  Result := TdxPasteSelectionCoreCommand.Create(RichEditControl, ASource);
end;

class function TdxPasteSelectionCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteDescription);
end;

function TdxPasteSelectionCommand.GetFormat: TdxRichEditDocumentFormat;
var
  ACommand: TdxPasteSelectionCoreCommand;
begin
  ACommand := TdxPasteSelectionCoreCommand(InsertObjectCommand);
  Result := ACommand.Format;
end;

class function TdxPasteSelectionCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxPasteSelectionCoreCommand;
end;

class function TdxPasteSelectionCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteMenuCaption);
end;

class function TdxPasteSelectionCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.PasteSelection;
end;

class function TdxPasteSelectionCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.PasteSelection;
end;

function TdxPasteSelectionCommand.GetKeepLastParagraphMarkInSelection: Boolean;
begin
  Result := False;
end;

procedure TdxPasteSelectionCommand.SetFormat(const Value: TdxRichEditDocumentFormat);
var
  ACommand: TdxPasteSelectionCoreCommand;
begin
  ACommand := TdxPasteSelectionCoreCommand(InsertObjectCommand);
  ACommand.Format := Value;
end;

procedure TdxPasteSelectionCommand.UpdateUIState(
  const AState: IdxCommandUIState);
var
  APasteSelectionCommand: TdxCommand;
  ACommandState: IdxCommandUIState;
begin
  APasteSelectionCommand := InsertObjectCommand;
  ACommandState := APasteSelectionCommand.CreateDefaultCommandUIState;
  APasteSelectionCommand.UpdateUIState(ACommandState);
  if ACommandState.Enabled and ACommandState.Visible then
  begin
    ApplyCommandRestrictionOnEditableControl(AState, Options.Behavior.Paste, ACommandState.Enabled);
    ApplyDocumentProtectionToSelectedCharacters(AState);
  end
  else
  begin
    AState.Enabled := False;
    AState.Visible := ACommandState.Visible;
    ApplyCommandRestrictionOnEditableControl(AState, Options.Behavior.Paste, AState.Enabled);
  end;
  AState.Checked := ACommandState.Checked;
end;

procedure TdxPasteSelectionCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
begin
  Assert(Commands.Count = 2);
  InsertObjectCommand.UpdateUIState(AState);
end;

{ TdxPasteSelectionCoreCommand }

constructor TdxPasteSelectionCoreCommand.Create(
  const ARichEditControl: IdxRichEditControl; const APasteSource: IdxPasteSource);
begin
  FPasteSource := APasteSource;
  inherited Create(ARichEditControl);
  AssignPasteSource;
end;

procedure TdxPasteSelectionCoreCommand.AddCommand(ACommandClass: TdxPasteContentCommandBaseClass);
var
  ACommand: TdxPasteContentCommandBase;
begin
  if ACommandClass = nil then
    Exit;
  ACommand := ACommandClass.Create(RichEditControl);
  if CanAddCommandFormat(ACommand) then
    Commands.Add(ACommand)
  else
    ACommand.Free;
end;

procedure TdxPasteSelectionCoreCommand.AssignPasteSource;
var
  ACount: Integer;
  I: Integer;
  ACommand: TdxPasteContentCommandBase;
begin
  ACount := Commands.Count;
  for I := 0 to ACount - 1 do
  begin
    ACommand := Safe<TdxPasteContentCommandBase>.Cast(Commands[I]);
    if ACommand <> nil then
      ACommand.PasteSource := PasteSource;
  end;
end;

function TdxPasteSelectionCoreCommand.CanAddCommandFormat(ACommand: TdxPasteContentCommandBase): Boolean;
begin
  Result := (Format = TdxRichEditDocumentFormat.Undefined) or (Format = ACommand.Format);
end;

function TdxPasteSelectionCoreCommand.ContainsData(
  const AFormat: string): Boolean;
begin
  Result := FPasteSource.ContainsData(AFormat);
end;

function TdxPasteSelectionCoreCommand.PasteFromIE: Boolean;
begin
  Result := ContainsData(TdxOfficeDataFormats.MsSourceUrl);
end;

procedure TdxPasteSelectionCoreCommand.CreateCommands;
begin
  if ContainsData(TdxOfficeDataFormats.XMLSpreadsheet) or PasteFromIE then
  begin
    AddCommand(dxPasteHtmlTextCommandClass);
    AddCommand(TdxPasteRtfTextCommand);
  end
  else
  begin
    AddCommand(TdxPasteRtfTextCommand);
    AddCommand(dxPasteHtmlTextCommandClass);
  end;
  AddCommand(TdxPastePlainTextCommand);
  AddCommand(TdxPasteImageCommand);
  AddCommand(TdxPasteMetafileCommand);
  AddCommand(TdxPasteImagesFromFilesCommand);
end;

function TdxPasteSelectionCoreCommand.ExecuteCommand(ACommand: TdxCommand;
  const AState: IdxCommandUIState): Boolean;
begin
  try
    FPasteException := nil;
    inherited ExecuteCommand(ACommand, AState);
    Result := True;
  except
    on E: Exception do
    begin
      if FPasteException = nil then
        FPasteException := E;
      Result := False;
    end;
  end;
end;

function TdxPasteSelectionCoreCommand.GetExecutionMode: TdxMultiCommandExecutionMode;
begin
  Result := TdxMultiCommandExecutionMode.ExecuteFirstAvailable;
end;

procedure TdxPasteSelectionCoreCommand.ForceExecuteCore(
  const AState: IdxCommandUIState);
begin
  FPasteException := nil;
  try
    inherited ForceExecuteCore(AState);
    if FPasteException <> nil then
    begin
      Assert(False);
    end;
  finally
    FPasteException := nil;
  end;
end;

class function TdxPasteSelectionCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteDescription);
end;

class function TdxPasteSelectionCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteMenuCaption);
end;

procedure TdxPasteSelectionCoreCommand.SetFormat(
  const Value: TdxRichEditDocumentFormat);
begin
  if FFormat <> Value then
  begin
    FFormat := Value;
    Commands.Clear;
    CreateCommands;
    AssignPasteSource;
  end;
end;

function TdxPasteSelectionCoreCommand.GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode;
begin
  Result := TdxMultiCommandUpdateUIStateMode.EnableIfAnyAvailable;
end;

{ TdxCopySelectionCommand }

constructor TdxCopySelectionCommand.Create(const ARichEditControl: IdxRichEditControl);
begin
  inherited Create(ARichEditControl);
  FCopySelectionManager := CreateCopySelectionManager;
end;

function TdxCopySelectionCommand.CreateCopySelectionManager: TdxCopySelectionManager;
begin
  Result := InnerControl.CreateCopySelectionManager;
end;

destructor TdxCopySelectionCommand.Destroy;
begin
  FreeAndNil(FCopySelectionManager);
  inherited;
end;

procedure TdxCopySelectionCommand.ExecuteCore;
var
  ASelection: TdxSelection;
  ACollection: TdxSelectionRangeCollection;
begin
  ASelection := DocumentModel.Selection;
  ACollection := ASelection.GetSortedSelectionCollection;
  try
    CopySelectionManager.CopyDocumentRange(ActivePieceTable, ACollection);
  finally
    ACollection.Free;
  end;
end;

function TdxCopySelectionCommand.GetDefaultPropertiesCopyOptions: TdxDefaultPropertiesCopyOptions;
begin
  Result := CopySelectionManager.DefaultPropertiesCopyOptions;
end;

class function TdxCopySelectionCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandCopySelectionDescription);
end;

function TdxCopySelectionCommand.GetFixLastParagraph: Boolean;
begin
  Result := CopySelectionManager.FixLastParagraph;
end;

class function TdxCopySelectionCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandCopySelectionMenuCaption);
end;

class function TdxCopySelectionCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.CopySelection;
end;

class function TdxCopySelectionCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.CopySelection;
end;

procedure TdxCopySelectionCommand.SetDefaultPropertiesCopyOptions(
  const Value: TdxDefaultPropertiesCopyOptions);
begin
  CopySelectionManager.DefaultPropertiesCopyOptions := Value;
end;

procedure TdxCopySelectionCommand.SetFixLastParagraph(const Value: Boolean);
begin
  CopySelectionManager.FixLastParagraph := Value;
end;

procedure TdxCopySelectionCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Checked := False;
  ApplyCommandsRestriction(AState, Options.Behavior.Copy, DocumentModel.Selection.Length > 0);
end;

end.
